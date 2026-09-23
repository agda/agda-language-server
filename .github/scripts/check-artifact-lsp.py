#!/usr/bin/env python3
"""Checks that a packaged 'als' binary can complete a real Agda load.

Unlike a smoke test that only waits for the immediate LSP acknowledgement,
this probe keeps talking to the server until Agda actually finishes
checking the file (or reports an error), which is the only way to catch
packaging bugs where 'als' silently falls back to a data directory that
only exists on the machine that built it (#30).

Usage:
  check-artifact-lsp.py <path-to-als> <path-to-fixture.agda> [--timeout SECONDS]

Uses only the Python standard library. Does not import project code or
invoke GHC, Cabal, Stack, or the Haskell test executable.
"""

import argparse
import json
import os
import queue
import subprocess
import sys
import tempfile
import threading
import time
from pathlib import Path

DEFAULT_TIMEOUT = 120.0


class ProbeFailure(Exception):
    pass


def log(msg):
    print(msg, file=sys.stderr, flush=True)


def send_message(proc, obj):
    body = json.dumps(obj).encode("utf-8")
    header = ("Content-Length: %d\r\n\r\n" % len(body)).encode("ascii")
    try:
        proc.stdin.write(header)
        proc.stdin.write(body)
        proc.stdin.flush()
    except (BrokenPipeError, OSError) as e:
        raise ProbeFailure("failed to write to the server's stdin: %s" % e)


def reader_thread(stdout, msg_queue, stop_event):
    try:
        while not stop_event.is_set():
            header_bytes = b""
            while b"\r\n\r\n" not in header_bytes:
                chunk = stdout.read(1)
                if not chunk:
                    msg_queue.put(("eof", None))
                    return
                header_bytes += chunk
            headers_part, _, rest_after = header_bytes.partition(b"\r\n\r\n")
            headers = {}
            for line in headers_part.split(b"\r\n"):
                if not line:
                    continue
                k, _, v = line.partition(b":")
                headers[k.strip().lower()] = v.strip()
            length_raw = headers.get(b"content-length")
            if length_raw is None:
                msg_queue.put(("malformed", "message with no Content-Length header"))
                return
            try:
                length = int(length_raw)
            except ValueError:
                msg_queue.put(("malformed", "non-integer Content-Length: %r" % length_raw))
                return
            body = rest_after
            while len(body) < length:
                chunk = stdout.read(length - len(body))
                if not chunk:
                    msg_queue.put(("eof", None))
                    return
                body += chunk
            try:
                obj = json.loads(body.decode("utf-8"))
            except Exception as e:
                msg_queue.put(("malformed", "invalid JSON: %s" % e))
                return
            msg_queue.put(("message", obj))
    except Exception as e:
        msg_queue.put(("error", str(e)))


def stderr_thread(stderr, lines, lock):
    for raw_line in iter(stderr.readline, b""):
        with lock:
            lines.append(raw_line.decode("utf-8", "replace").rstrip("\n"))
            del lines[:-200]
    try:
        stderr.close()
    except OSError:
        pass


class Probe:
    def __init__(self, als_path, fixture_path, timeout):
        self.als_path = als_path
        self.fixture_path = fixture_path
        self.timeout = timeout
        self.deadline = time.monotonic() + timeout
        self.msg_queue = queue.Queue()
        self.stderr_lines = []
        self.stderr_lock = threading.Lock()
        self.protocol_log = []
        self.next_id = 1
        self.proc = None
        self.stop_event = threading.Event()

    def remaining(self):
        return max(0.0, self.deadline - time.monotonic())

    def record(self, direction, obj):
        self.protocol_log.append("%s %s" % (direction, json.dumps(obj)[:500]))
        del self.protocol_log[:-50]

    def send(self, obj):
        self.record(">>>", obj)
        send_message(self.proc, obj)

    def next_request_id(self):
        i = self.next_id
        self.next_id += 1
        return i

    def receive(self):
        try:
            kind, payload = self.msg_queue.get(timeout=self.remaining())
        except queue.Empty:
            raise ProbeFailure(
                "timed out after %.0fs waiting for a message from the server" % self.timeout
            )
        if kind == "eof":
            raise ProbeFailure("server closed stdout unexpectedly (EOF)")
        if kind == "malformed":
            raise ProbeFailure("malformed message framing/JSON from server: %s" % payload)
        if kind == "error":
            raise ProbeFailure("error reading from server: %s" % payload)
        self.record("<<<", payload)
        return payload

    def run(self):
        self.check_layout()
        env = self.build_env()
        run_dir = tempfile.mkdtemp(prefix="als-artifact-probe-")
        log("starting %s in %s" % (self.als_path, run_dir))
        self.proc = subprocess.Popen(
            [self.als_path],
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            cwd=run_dir,
            env=env,
        )
        reader = threading.Thread(
            target=reader_thread, args=(self.proc.stdout, self.msg_queue, self.stop_event), daemon=True
        )
        reader.start()
        err_thread = threading.Thread(
            target=stderr_thread, args=(self.proc.stderr, self.stderr_lines, self.stderr_lock), daemon=True
        )
        err_thread.start()

        try:
            self.initialize()
            self.load_fixture()
            self.wait_for_completion()
        finally:
            self.stop_event.set()

    def check_layout(self):
        als_abs = os.path.abspath(self.als_path)
        if not os.path.isfile(als_abs):
            raise ProbeFailure("executable not found: %s" % als_abs)
        if not os.access(als_abs, os.X_OK):
            raise ProbeFailure("executable is not executable: %s" % als_abs)
        self.als_path = als_abs

        fixture_abs = os.path.abspath(self.fixture_path)
        if not os.path.isfile(fixture_abs):
            raise ProbeFailure("fixture not found: %s" % fixture_abs)
        self.fixture_path = Path(fixture_abs)

        data_dir = os.path.join(os.path.dirname(als_abs), "data")
        lib_dir = os.path.join(data_dir, "lib")
        if not os.path.isdir(lib_dir):
            raise ProbeFailure(
                "packaging problem, not a runtime lookup failure: "
                "expected a sibling 'data/lib' directory at %s" % lib_dir
            )

    def build_env(self):
        env = dict(os.environ)
        for k in list(env.keys()):
            if k.lower() == "agda_datadir":
                del env[k]
        agda_dir = tempfile.mkdtemp(prefix="als-artifact-probe-agda-dir-")
        env["AGDA_DIR"] = agda_dir
        return env

    def initialize(self):
        init_id = self.next_request_id()
        root_uri = self.fixture_path.parent.as_uri()
        self.send(
            {
                "jsonrpc": "2.0",
                "id": init_id,
                "method": "initialize",
                "params": {
                    "processId": os.getpid(),
                    "rootUri": root_uri,
                    "rootPath": str(self.fixture_path.parent),
                    "capabilities": {},
                },
            }
        )
        while True:
            msg = self.receive()
            if msg.get("id") == init_id and "method" not in msg:
                if "error" in msg:
                    raise ProbeFailure("initialize failed: %s" % msg["error"])
                break
            # ignore anything else that might arrive before the response
        self.send({"jsonrpc": "2.0", "method": "initialized", "params": {}})

    def load_fixture(self):
        fixture_posix = self.fixture_path.as_posix()
        iotcm = 'IOTCM "%s" NonInteractive Direct( Cmd_load "%s" [] )' % (
            fixture_posix,
            fixture_posix,
        )
        load_id = self.next_request_id()
        self.send(
            {
                "jsonrpc": "2.0",
                "id": load_id,
                "method": "agda",
                "params": {"tag": "CmdReq", "contents": iotcm},
            }
        )
        self.load_id = load_id

    def wait_for_completion(self):
        while True:
            msg = self.handle_one_message(expect_load_ack=hasattr(self, "load_id"))
            if msg == "done":
                break

    def handle_one_message(self, expect_load_ack):
        msg = self.receive()

        # response to a request we sent
        if "id" in msg and "method" not in msg:
            if expect_load_ack and msg.get("id") == self.load_id:
                del self.load_id
                if "error" in msg:
                    raise ProbeFailure("Cmd_load request failed: %s" % msg["error"])
                result = msg.get("result")
                if result != {"tag": "CmdRes", "contents": None}:
                    raise ProbeFailure(
                        "expected 'CmdRes Nothing' acknowledgement, got: %s" % json.dumps(result)
                    )
            return None

        # server-initiated request: must always reply so the server can proceed
        if "id" in msg and "method" in msg:
            reply_id = msg["id"]
            self.send({"jsonrpc": "2.0", "id": reply_id, "result": None})
            if msg.get("method") != "agda":
                return None
            return self.inspect_agda_response(msg.get("params"))

        # notification (no id): ignore (log output, progress, etc.)
        return None

    def inspect_agda_response(self, params):
        if not isinstance(params, dict):
            return None
        if params.get("tag") != "ResponseDisplayInfo":
            return None
        info = params.get("contents")
        if not isinstance(info, dict):
            return None
        tag = info.get("tag")
        if tag == "DisplayInfoError":
            raise ProbeFailure("Agda reported DisplayInfoError: %s" % info.get("contents"))
        if tag == "DisplayInfoAllGoalsWarnings":
            contents = info.get("contents")
            if not isinstance(contents, list) or len(contents) != 5:
                raise ProbeFailure(
                    "unexpected DisplayInfoAllGoalsWarnings shape: %s" % json.dumps(info)
                )
            title, _visible, _invisible, _warnings, errors = contents
            if title == "*All Done*" and errors == []:
                return "done"
            raise ProbeFailure(
                "Agda finished with unexpected result (title=%r, errors=%s)" % (title, errors)
            )
        return None

    def shutdown(self):
        try:
            shutdown_id = self.next_request_id()
            self.send({"jsonrpc": "2.0", "id": shutdown_id, "method": "shutdown", "params": None})
            deadline = time.monotonic() + 5.0
            while time.monotonic() < deadline:
                remaining = deadline - time.monotonic()
                try:
                    kind, payload = self.msg_queue.get(timeout=max(0.0, remaining))
                except queue.Empty:
                    break
                if kind == "message" and payload.get("id") == shutdown_id:
                    break
            self.send({"jsonrpc": "2.0", "method": "exit"})
        except Exception:
            pass  # best-effort only; failure here must not mask a passing run

    def terminate(self):
        if self.proc is None:
            return
        if self.proc.poll() is None:
            try:
                self.proc.terminate()
                self.proc.wait(timeout=5)
            except Exception:
                try:
                    self.proc.kill()
                except Exception:
                    pass

    def dump_diagnostics(self):
        log("--- recent protocol messages ---")
        for line in self.protocol_log:
            log(line)
        log("--- recent stderr ---")
        with self.stderr_lock:
            for line in self.stderr_lines:
                log(line)
        log("---------------------------------")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("als_path", help="path to the extracted als/als.exe")
    parser.add_argument("fixture_path", help="path to a small checked-in Agda fixture")
    parser.add_argument(
        "--timeout", type=float, default=DEFAULT_TIMEOUT, help="timeout in seconds (default: %(default)s)"
    )
    args = parser.parse_args()

    probe = Probe(args.als_path, args.fixture_path, args.timeout)
    try:
        probe.run()
    except ProbeFailure as e:
        log("FAIL: %s" % e)
        probe.dump_diagnostics()
        probe.terminate()
        sys.exit(1)
    except Exception as e:
        log("FAIL: unexpected error: %s" % e)
        probe.dump_diagnostics()
        probe.terminate()
        sys.exit(1)

    probe.shutdown()
    probe.terminate()
    log("PASS: %s completed Cmd_load on %s with '*All Done*'" % (args.als_path, args.fixture_path))
    sys.exit(0)


if __name__ == "__main__":
    main()

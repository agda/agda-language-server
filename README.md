# agda-language-server
Language Server Protocol for Agda

## Development

### macOs

You would probably need to run this first:

```bash
make icu-macos-fix
```

and supply these two arguments when using `stack`:

* `--extra-lib-dirs=/usr/local/opt/icu4c/lib`
* `--extra-include-dirs=/usr/local/opt/icu4c/include`

for example:

```bash
stack repl \
   --extra-lib-dirs=/usr/local/opt/icu4c/lib         \
   --extra-include-dirs=/usr/local/opt/icu4c/include
```

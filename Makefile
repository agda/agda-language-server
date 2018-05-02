BASEDIR=$(CURDIR)

repl:
		stack repl \
			 --extra-lib-dirs=/usr/local/opt/icu4c/lib         \
			 --extra-include-dirs=/usr/local/opt/icu4c/include
.PHONY: repl

icu-macos-fix:
	brew install icu4c                                     \
	&& stack --stack-yaml=stack.yaml build text-icu  \
         --extra-lib-dirs=/usr/local/opt/icu4c/lib         \
         --extra-include-dirs=/usr/local/opt/icu4c/include
.PHONY: icu-macos-fix

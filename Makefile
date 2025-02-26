INSTALL_ARGS := $(if $(PREFIX),--prefix $(PREFIX),)

lock: ## Generate a lock file
	opam lock . -y
# Default rule
default: ## Default rule, build install
	dune build @install

install:
	dune install $(INSTALL_ARGS)

uninstall:
	dune uninstall $(INSTALL_ARGS)

reinstall: uninstall install

test:
	dune build @runtest

example:
	dune build examples/server.exe

clean:
	rm -rf _build

.PHONY: default install uninstall reinstall clean test

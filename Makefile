# Dumb Build Rule, Rely on Stack+Cargo to efficiently not rebuild.
# ------------------------------------------------------------------------------
.PHONY: build
build:
	pushd abci-shim && cargo build && popd
	stack build

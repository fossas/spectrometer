FMT_OPTS = -co -XTypeApplications -o -XImportQualifiedPost
FIND_OPTS = src test -type f -name '*.hs'

build:
	cabal build

test:
	cabal test

# Dogfood the dev version
analyze:
	cabal run fossa -- analyze --output --debug --filter 'cabal@./'

# Copy the built binary into the local root
fossa:
	cp $(shell cabal list-bin fossa) ./fossa

install-local: fossa

# Run analysis on the sandbox directory
sandbox: fossa
	./fossa analyze --output --debug --record ./sandbox > fossa.json

# Run the sandbox with replay mode on
replay: fossa fossa.debug.json
	./fossa analyze --output --debug --replay fossa.debug.json ./sandbox > fossa.replay.json

# Format everything
# `@command` does not echo the command before running
fmt:
	@fourmolu --mode inplace ${FMT_OPTS} $(shell find ${FIND_OPTS})

# Confirm everything is formatted without changing anything
fmt-ck:
	@fourmolu --mode check ${FMT_OPTS} $(shell find ${FIND_OPTS})
	@echo "No formatting errors found"

# Lint everything
lint:
	hlint src test

.PHONY: build test analyze install-local sandbox replay fmt fmt-ck lint
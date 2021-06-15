FMT_OPTS = -co -XTypeApplications -o -XImportQualifiedPost 

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

# 
fmt:
	fourmolu --mode inplace ${FMT_OPTS} $(shell fd --type f '\.hs$$')

fmt-ck:
	fourmolu --mode check ${FMT_OPT} $(shell fd --type f '\.hs$$')

lint:
	hlint src test

.PHONY: build test analyze install-local sandbox replay fmt fmt-ck lint
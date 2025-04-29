# Having this here is a little goofy, but it's really more for the management
# of the *project* than actually doing any building/compiling. That's what
# stack does, so why step on those toes?
PROJECT = compute-optical-potential
installLocation = ${HOME}/.local/bin/

.PHONY: clean purge project go release all dryrun uninstall

project:
	stack build

release:
	stack build --ghc-options="-O3"

all:
	make release
	stack install --local-bin-path ${installLocation}

dryrun:
	stack install --dry-run --local-bin-path ${installLocation}
	@echo "Would install as: ${installLocation}${PROJECT}"

uninstall:
	rm ${installLocation}/${PROJECT}

clean:
	stack clean

purge:
	stack purge

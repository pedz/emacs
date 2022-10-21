.DEFAULT_GOAL := all
DRONES_DIR = $(shell git config "borg.drones-directory" || echo "lib")

# Comment this out or make it more intelligent for non-Mac platforms.
EMACS=/Applications/Emacs.app/Contents/MacOS/Emacs

all: clean build

-include $(DRONES_DIR)/borg/borg.mk

bootstrap-borg:
	@git submodule--helper clone --name borg --path $(DRONES_DIR)/borg \
	--url git@github.com:emacscollective/borg.git
	@cd $(DRONES_DIR)/borg; git symbolic-ref HEAD refs/heads/master
	@cd $(DRONES_DIR)/borg; git reset --hard HEAD

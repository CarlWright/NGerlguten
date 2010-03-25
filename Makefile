SUBDIRS = src priv/fonts/bin

.PHONY: all conf $(SUBDIRS)

all: conf $(SUBDIRS)

conf:
	cd $@ && $(MAKE)

$(SUBDIRS):
	cd $@ && $(MAKE)

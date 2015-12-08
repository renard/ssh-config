IMAGE_BUILDER_URL = https://raw.githubusercontent.com/renard/cl-image-builder/dev/image-builder.lisp
BUILDDIR      = build
IMAGE_BUILDER = $(BUILDDIR)/image-builder.lisp
#IMAGE_BUILDER = ../image-builder/image-builder.lisp
ASDF          = $(wildcard *.asd)
APP           = $(patsubst %.asd,%,$(ASDF))


all: $(APP)

$(BUILDDIR):
	mkdir -p $@

$(IMAGE_BUILDER): $(BUILDDIR)
	curl -o $@ https://raw.githubusercontent.com/renard/cl-image-builder/master/image-builder.lisp


sbcl: $(APP).sbcl.exe
$(APP).sbcl.exe: $(IMAGE_BUILDER)
	sbcl --no-sysinit --no-userinit \
		--load $(IMAGE_BUILDER) \
		--eval '(image-builder:build-image)' 

ccl: $(APP).ccl.exe
$(APP).ccl.exe: $(IMAGE_BUILDER)
	ccl64 -n -l $(IMAGE_BUILDER) \
		--eval '(image-builder:build-image)'

$(APP): $(APP).sbcl.exe $(APP).ccl.exe;
app: sbcl ccl


clean:
	rm -fr $(BUILDDIR)

.PHONY: clean

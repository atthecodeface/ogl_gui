# This makefile supports a server which uses 'grip', which requires 'pip install grip'
# If you run 'make grip_server' then it will start an HTTP server accessible from
# http://localhost:6419/README.md

# This makefile also uses pandoc to batch convert markdown to HTML
DOCS = opengl.html \
	README.html \
	opengl_gui.html

BUILT_DOCS := $(foreach doc,$(DOCS),build/$(doc))

all: $(BUILT_DOCS)

grip_server:
	grip .

build/%.html: %.md
	pandoc -o $@ $<

clean:
	rm -rf *~ $(BUILT_DOCS)
	mkdir -p build

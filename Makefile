

OPAMROOT = ~/.opam/system/bin/
OPAMLIB = ~/.opam/system/lib
BREWROOT = ~/Git/brew/
OCAMLROOT = $(BREWROOT)/bin/

SETPATH := PATH=$(OCAMLROOT):$(OPAMROOT):$(PATH) 
OCAMLFIND  = $(SETPATH) $(OPAMROOT)/ocamlfind
OCAMLC     = $(SETPATH) $(OCAMLROOT)/ocamlc

ATCFDIR := /Users/gavinprivate/Git/atcflib
ATCFOCAML := ${ATCFDIR}/ocaml
LIBPATH := DYLD_LIBRARY_PATH=.:${ATCFDIR}:${ATCFOCAML}
ATCFCMXA  := ${ATCFOCAML}/atcflib.cmx ${ATCFOCAML}/ocaml_atcflib.a

OCAML_COMPILE := $(OCAMLFIND) ocamlopt

PROGS := font.cmx sax.cmx stylesheet.cmx animatable.cmx ogl_program.cmx ogl_obj_standard.cmx ogl_obj_standard.cmi ogl_types.cmi ogl_layout.cmx ogl_decoration.cmx ogl_widget.cmx ogl_app.cmx sdl_ogl_gui.cmxa plot_obj

all:  $(PROGS) 

.PHONY:show_mli
show_mli:
	@$(OCAMLFIND) ocamlopt -I ${ATCFOCAML} ${OGL_GUI_PACKAGES} ${ATCFCMXA} sdl_ogl_gui.ml -i

#.PHONY: create_index
#create_index: ./create_index

DEVANAL_PACKAGES := -linkpkg -package bigarray,re -thread

OGL_GUI_PACKAGES := -linkpkg -package bigarray,re,tgls,tgls.tgl4,tsdl,yojson -thread

UTILS_PACKAGES := -linkpkg -package bigarray,re,tgls,tgls.tgl4,tsdl,yojson -thread

MKPNG_PACKAGES := -linkpkg -package imagelib,re

utils.cmx: utils.ml utils.mli
	@echo "Compile utils.mli to create .cmi"
	@$(OCAMLFIND) ocamlopt  -I ${ATCFOCAML} ${UTILS_PACKAGES} -c utils.mli
	@echo "Compile utils.ml to create .cmx and .o"
	@$(OCAMLFIND) ocamlopt  -I ${ATCFOCAML} ${UTILS_PACKAGES} -c utils.ml

sax.cmx: sax.ml sax.mli
	@echo "Compile sax.mli to create .cmi"
	@$(OCAMLFIND) ocamlopt  -I ${ATCFOCAML} ${SAX_PACKAGES} -c sax.mli
	@echo "Compile sax.ml to create .cmx and .o"
	@$(OCAMLFIND) ocamlopt  -I ${ATCFOCAML} ${SAX_PACKAGES} -c sax.ml

font.cmx: font.ml font.mli utils.cmx ${ATCFMXA}
	@echo "Compile font.mli to create .cmi"
	$(OCAMLFIND) ocamlopt  ${OGL_GUI_PACKAGES} -c font.mli
	@echo "Compile font.ml to create .cmx and .o"
	@$(OCAMLFIND) ocamlopt -I ${ATCFOCAML} ${OGL_GUI_PACKAGES} -c font.ml

stylesheet.cmx: stylesheet.ml stylesheet.mli utils.cmx ${ATCFMXA}
	@echo "Compile stylesheet.mli to create .cmi"
	@$(OCAMLFIND) ocamlopt  -I ${ATCFOCAML} ${OGL_GUI_PACKAGES} -c stylesheet.mli
	@echo "Compile stylesheet.ml to create .cmx and .o"
	@$(OCAMLFIND) ocamlopt -I ${ATCFOCAML} ${OGL_GUI_PACKAGES} -c stylesheet.ml

animatable.cmx: animatable.ml animatable.mli utils.cmx ${ATCFMXA}
	@echo "Compile animatable.mli to create .cmi"
	@$(OCAMLFIND) ocamlopt  -I ${ATCFOCAML} ${OGL_GUI_PACKAGES} -c animatable.mli
	@echo "Compile animatable.ml to create .cmx and .o"
	@$(OCAMLFIND) ocamlopt -I ${ATCFOCAML} ${OGL_GUI_PACKAGES} -c animatable.ml

ogl_program.cmx: ogl_program.ml ogl_program.mli utils.cmx ${ATCFMXA}
	@echo "Compile ogl_program.mli to create .cmi"
	@$(OCAMLFIND) ocamlopt  -I ${ATCFOCAML} ${OGL_GUI_PACKAGES} -c ogl_program.mli
	@echo "Compile ogl_program.ml to create .cmx and .o"
	@$(OCAMLFIND) ocamlopt -I ${ATCFOCAML} ${OGL_GUI_PACKAGES} -c ogl_program.ml

ogl_layout.cmx: ogl_layout.ml ogl_layout.mli utils.cmx ${ATCFMXA}
	@echo "Compile ogl_layout.mli to create .cmi"
	@$(OCAMLFIND) ocamlopt  -I ${ATCFOCAML} ${OGL_GUI_PACKAGES} -c ogl_layout.mli
	@echo "Compile ogl_layout.ml to create .cmx and .o"
	@$(OCAMLFIND) ocamlopt -I ${ATCFOCAML} ${OGL_GUI_PACKAGES} -c ogl_layout.ml

ogl_decoration.cmx: ogl_decoration.ml ogl_decoration.mli  ${ATCFMXA}
	@echo "Compile ogl_decoration.mli to create .cmi"
	@$(OCAMLFIND) ocamlopt  -I ${ATCFOCAML} ${OGL_GUI_PACKAGES} -c ogl_decoration.mli
	@echo "Compile ogl_decoration.ml to create .cmx and .o"
	@$(OCAMLFIND) ocamlopt -I ${ATCFOCAML} ${OGL_GUI_PACKAGES} -c ogl_decoration.ml

ogl_types.cmi: ogl_types.mli
	@echo "Compile ogl_types.mli to create .cmi"
	@$(OCAMLFIND) ocamlopt   -I ${ATCFOCAML}  ${OGL_GUI_PACKAGES} -opaque -c ogl_types.mli

ogl_obj_standard.cmx: ogl_obj.ml ogl_obj.mli ogl_obj_geometry.ml ogl_obj_text.ml utils.cmx ogl_types.cmi ${ATCFMXA}
	@echo "Compile ogl_obj_standard.mli to create .cmi"
	@$(OCAMLFIND) ocamlopt   -I ${ATCFOCAML}  ${OGL_GUI_PACKAGES} -c ogl_obj_standard.mli
	@echo "Compile ogl_obj_text.mli to create .cmi"
	@$(OCAMLFIND) ocamlopt   -I ${ATCFOCAML}  ${OGL_GUI_PACKAGES} -c ogl_obj_text.mli
	@echo "Compile ogl_obj_geometry.mli to create .cmi"
	@$(OCAMLFIND) ocamlopt   -I ${ATCFOCAML}  ${OGL_GUI_PACKAGES} -c ogl_obj_geometry.mli
	@$(OCAMLFIND) ocamlopt -I ${ATCFOCAML} ${OGL_GUI_PACKAGES} -c ogl_obj_standard.ml  -o ogl_obj_standard.cmx
	@echo "Compile ogl_obj.mli to create .cmi"
	@$(OCAMLFIND) ocamlopt  ${OGL_GUI_PACKAGES} -c ogl_obj.mli
	@echo "Compile ogl_obj.ml, ogl_obj_geometry.ml, ogl_obj_text to create .cmx and .o"
	@$(OCAMLFIND) ocamlopt -I ${ATCFOCAML} ${OGL_GUI_PACKAGES} -c ogl_obj.ml           -o ogl_obj.cmx
	@$(OCAMLFIND) ocamlopt -I ${ATCFOCAML} ${OGL_GUI_PACKAGES} -c ogl_obj_geometry.ml  -o ogl_obj_geometry.cmx
	@$(OCAMLFIND) ocamlopt -I ${ATCFOCAML} ${OGL_GUI_PACKAGES} -c ogl_obj_text.ml      -o ogl_obj_text.cmx
	@echo "Archive ogl_obj.cmx ogl_obj_geometry.cmx, ogl_obj_text.cmx to create ogl_obj_standard.cmxa"
	@$(OCAMLFIND) ocamlopt -a -o ogl_obj_standard.cmxa ogl_obj.cmx ogl_obj_geometry.cmx ogl_obj_text.cmx 


ogl_widget.cmx: ogl_widget.ml ogl_widget.mli utils.cmx font.cmx ogl_obj_standard.cmxa ${ATCFMXA}
	@echo "Compile ogl_widget.mli to create .cmi"
	@$(OCAMLFIND) ocamlopt   -I ${ATCFOCAML} ${OGL_GUI_PACKAGES} -c ogl_widget.mli
	@echo "Compile ogl_widget.ml to create .cmx and .o"
	@$(OCAMLFIND) ocamlopt   -I ${ATCFOCAML} ${OGL_GUI_PACKAGES} -c ogl_widget.ml

ogl_app.cmx: ogl_app.ml ogl_app.mli utils.cmx font.cmx ogl_obj_standard.cmxa ${ATCFMXA}
	@echo "Compile ogl_app.mli to create .cmi"
	@$(OCAMLFIND) ocamlopt   -I ${ATCFOCAML} ${OGL_GUI_PACKAGES} -c ogl_app.mli
	@echo "Compile ogl_app.ml to create .cmx and .o"
	@$(OCAMLFIND) ocamlopt   -I ${ATCFOCAML} ${OGL_GUI_PACKAGES} -c ogl_app.ml


SUBLIBRARIES := utils.cmx font.cmx stylesheet.cmx sax.cmx animatable.cmx ogl_program.cmx ogl_layout.cmx ogl_obj.cmx  ogl_obj_geometry.cmx ogl_obj_text.cmx ogl_obj_standard.cmx ogl_decoration.cmx ogl_widget.cmx ogl_app.cmx
sdl_ogl_gui.cmxa: sdl_ogl_gui.ml sdl_ogl_gui.mli ${ATCFMXA} ${SUBLIBRARIES}
	@echo "Compile sdl_ogl_gui.mli to create .cmi"
	@$(OCAMLFIND) ocamlopt   -I ${ATCFOCAML} ${OGL_GUI_PACKAGES} -c sdl_ogl_gui.mli
	@echo "Compile sdl_ogl_gui.ml to create .cmx and .o"
	@$(OCAMLFIND) ocamlopt   -I ${ATCFOCAML} ${OGL_GUI_PACKAGES}  ${SUBLIBRARIES} -c sdl_ogl_gui.ml 
	@echo "Build sdl_ogl_gui.cmxa"
	@$(OCAMLFIND) ocamlopt -a -o sdl_ogl_gui.cmxa  ${SUBLIBRARIES} sdl_ogl_gui.cmx

plot_obj: plot_obj.ml sdl_ogl_gui.cmxa 
	@echo "Build plot_obj"
	@$(OCAMLFIND) ocamlopt  -o plot_obj -I ${ATCFOCAML} ${OGL_GUI_PACKAGES}  ${ATCFCMXA} sdl_ogl_gui.cmxa plot_obj.ml

run: plot_obj
	${LIBPATH} ./plot_obj

clean:
	rm -f *.cmx *.cmi *.o *~ ${PROGS}



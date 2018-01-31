all:
	@echo "Use 'make build' to build the library"
	@echo "Use 'make install' to build and install the library using opam"
	@echo "Use 'make top' to build a toplevel and run it"

top:
	jbuilder utop src/top

clean:
	jbuilder clean

build:
	jbuilder build

install:
	jbuilder build @install
	jbuilder install

#SUBLIBRARIES := utils.cmx font.cmx stylesheet.cmx sax.cmx animatable.cmx ogl_program.cmx ogl_texture.cmx ogl_view.cmx ogl_layout.cmx ogl_obj.cmx  ogl_obj_geometry.cmx ogl_obj_text.cmx ogl_obj_standard.cmx ogl_decoration.cmx ogl_widget.cmx ogl_app.cmx
#sdl_ogl_gui.cmxa: sdl_ogl_gui.ml sdl_ogl_gui.mli ${ATCFMXA} ${SUBLIBRARIES}
#	@echo "Compile sdl_ogl_gui.mli to create .cmi"
#	@$(OCAMLFINDOPT)   -I ${ATCFOCAML} ${OGL_GUI_PACKAGES} -c sdl_ogl_gui.mli
#	@echo "Compile sdl_ogl_gui.ml to create .cmx and .o"
#	@$(OCAMLFINDOPT)   -I ${ATCFOCAML} ${OGL_GUI_PACKAGES}  ${SUBLIBRARIES} -c sdl_ogl_gui.ml 
#	@echo "Build sdl_ogl_gui.cmxa"
#	@$(OCAMLFINDOPT) -a -o sdl_ogl_gui.cmxa  ${SUBLIBRARIES} sdl_ogl_gui.cmx
#
#plot_obj: plot_obj.ml sdl_ogl_gui.cmxa 
#	@echo "Build plot_obj"
#	@$(OCAMLFINDOPT)  -o plot_obj -I ${ATCFOCAML} ${OGL_GUI_PACKAGES}  ${ATCFCMXA} sdl_ogl_gui.cmxa plot_obj.ml
#
#run: plot_obj
#	${LIBPATH} ./plot_obj
#
#clean:
#	rm -f *.cmx *.cmi *.o *~ ${PROGS}

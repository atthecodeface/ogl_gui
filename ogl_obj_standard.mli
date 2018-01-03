class virtual ogl_obj : Ogl_obj.ogl_obj
class ogl_obj_geometry :
  Tgl4.Gl.enum ->
  int ->
  int array ->
  Utils.float32_bigarray list -> Ogl_obj_geometry.ogl_obj_geometry

class ogl_obj_text :
  ?size:float ->
  ?height:float ->
  Font.Outline.t ->
  string -> Ogl_obj_text.ogl_obj_text

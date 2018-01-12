open Utils
class t_ogl_view_set (app:Ogl_types.t_ogl_app) (display:Ogl_types.t_ogl_display) : Ogl_types.t_ogl_view_set= object
  method get_app = app
  method get_display = display
end
module Ogl_view = struct
    let create app display = new t_ogl_view_set app display
    let get_material t name = Some ((t#get_app)#get_material name)
    let set t opt_material transformation = 
      (t#get_display)#set_material opt_material transformation
  end

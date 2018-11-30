open Stylesheet

module Stylesheet      = Stylesheet
module Styleable_value = Styleable_value

module Utils      = Utils
module Types      = Ogl_types
module Texture    = Texture
module Layout     = Layout
module Program    = Glprogram
module Widget     = Widget
module View       = Ogl_view
module Obj        = Obj
module App        = App
module AppBuilder = App_builder.Builder

class  ogl_obj_geometry = Obj.ogl_obj_geometry
class virtual  ogl_obj  = Obj.ogl_obj

class  ogl_widget = Widget.ogl_widget

class  ogl_app    = App.ogl_app

let create_stylesheet = Styling.create_stylesheet
let widget_base_styles = Styling.widget_base_styles


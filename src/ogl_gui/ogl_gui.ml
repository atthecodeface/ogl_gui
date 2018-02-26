open Stylesheet
open Ogl_widget

module Stylesheet      = Stylesheet
module Styleable_value = Styleable_value

module Utils      = Utils
module Types      = Ogl_types
module Texture    = Texture
module Layout     = Layout
module Program    = Glprogram
module Widget     = Ogl_widget
module View       = Ogl_view
module Obj        = Ogl_obj
module App        = App
module AppBuilder = App_builder.Builder

class  ogl_obj_geometry = Obj.ogl_obj_geometry
class virtual  ogl_obj          = Obj.ogl_obj

class  ogl_widget = Widget.ogl_widget

class  ogl_app    = App.ogl_app

let create_stylesheet = Ogl_widget.create_stylesheet


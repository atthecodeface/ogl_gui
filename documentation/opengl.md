# OpenGL

The GUI uses a single OpenGL context

In mono, each window has an OpenGL frame buffer. In stereo, each
window has two OpenGL frame buffers.

# GUI Structure

An application has one or more windows.

## Windows

A window contains a number of portals, each of which contains widgets,
which may themselves contain widgets.

## Portals

A portal is a rectangular portion of the OpenGL frame buffer, and in
stereo a portal has an eye separation. A portal also has one (or two,
for stereo) projection matrices that supply a viewing frustrum.

Portals can be placed in a window such that they overlap; portals are
drawn in a defined order, and when a portal is rendered its frame
buffer region may have its depth buffer cleared, and/or its pixel
buffer cleared, or nothing cleared.

A portal has a specific renderer, that maps from buffers that provide
depth, color, view positions, world normals, and other material data,
to screen frame buffer pixels.

Using portals one can achieve effects such as a HUD that is 'on top'
in drawing space, but can be 'further away' in stereo depth, compared
to a surface viewer, for example.

A portal has an OpenGL coordinate system of (+-a,+-1,+-1) with 0<a<=1,
or (+-1, +-a, +-1) depending on the aspect ratio of its view port.


!!! Should portals be the toplevel objects as viewports?

If so, a portal has a 'realistic' shader, a viewport, a depth, and a
frustrum projection matrix.

More than one portal may be overlaid - they are drawn in painter's
order, and the depth buffer for the view port for the portal is
cleared before drawing each one.

This would permit a HUD portal to be drawn at a different stereo depth to a
view portal

A HUD portal could also be rendered with an alpha blending to be
transparent or translucent.

A portal is rendered first as GUI elements to a set of
vertex/normal/material/... buffers, which can then be rendered with a
'realistic' shader, if desired.

The realistic shader can have a plurality of lighting sources, which
would not cast shadows, but which have specular lighting, Fresnel
effects, and atmospheric effects

So each portal has a realistic shader.


Multiple portals can be rendered in parallel. Their realistic shaders
can be run in series, from rearmost portal (in the hierarchy) to the frontmost.

## Widgets

Each widget has a size (dx,dy,dz) and a center (cx,cy,cz). The widget
is expected to fit within the region (cx+-dx/2, cy+-dy/x, cz+-dz/2).

The widget layout system is 3d-table based.

When a widget is 'laid out' it will be told its dimensions (dx,dy,dz)
and its center (cx,cy,cz).

A widget content will have a desired dimension, and during the layout
process these dimensions will be presented to the enclosing widget
(and portal), in determining the actual laid out content dimensions,
using an HTML5 style packing system.

### Widget decorations

A widget has some standard decorations; it has a border, and it has
faces. Since a widget is effectively a cuboid with dimensions
(dx,dy,dz), the contents are a cuboids of (possibly) smaller
dimensions. Between these two cuboids the border may be drawn; if no
border is specified, then the widget contents consume the whole of the
widget content cuboid. The widget content cuboid then has six faces,
and any of the faces may be defined to have a particular color or
material, which is drawn after the content (since the OpenGL is using
depth buffering, the background that is at-or-behind the content will
not overdraw the content).

# General portal pipeline

A portal is a 3D universe that may be realistically shaded.

From the outside in, a portal has a projection matrix P that maps the
content (+-1,+-1,+-1) to the projection frustrum
(+-1,+-1,clip,perspective).

The contents are widgets, which may have their own separate GUI matrices.

The internal of a portal is a set of objects in world space.

One set of objects may be rendered for more than one portal in a GUI.

An object is something that is drawn with a shader pipeline, usually
with an 'M' matrix applied to a standard coordinate system (e.g. a
cube may be +-1 in each coordinate, and M can place the cube at any
orientation and position in world space).

A list of objects is rendered through a view matrix 'V', which is
applied to world coorinates

## Vertex shader (without tessellation shader)

The vertex shader is reponsible for producing inputs to the fragment
pipeline. In the standard OpenGL model, these outputs are linearly
interpolated across polygons (hence the smaller the polygon, the
better the effects generally).

The vertex shader takes properties such as vertex, normal, and some concept of material
(color, texture and UV, glowiness, ...)

The vertex shader must take M, V, G and P, and apply M to get world
coordinates, V * M to get view coordinates, P * G * V * M to get portal coordinates.

For normals M is applied to (nx,ny,nx,0). For vertices, M is applied
to (vx,vy,vz,1)

For normals V * M is applied to (nx,ny,nx,0). For vertices, V * M is applied
to (vx,vy,vz,1)

Normals do not have G and P applied (since only world and view normals
are required For normals V * M is applied to (nx,ny,nx,0). For vertices, V * M is applied
to (vx,vy,vz,1)

## Vertex shader (with tessellation shader)

The vertex shader may be invoked with tessellation shading. This is a
mechanism where a vertex shader is invoked, producing view x, y and z
coordinates for vertices of polygons; the tessellation control shader
is then invoked, which decides how many triangles the polygon should
be broken up into for plotting (based, potentially, on the view x, y
and z coordinates); the hardware in the OpenGL pipeline breaks up the
polygons, and a tessellation evaluation shader is invoked for each
vertex of the new polygons to find the view x, y, and z coordinates
(and any other vertex shader outputs such as normals) that should be
used for these new polygons.

The vertex shader takes properties such as vertex, normal, and some concept of material
(color, texture and UV, glowiness, ...)

The vertex shader must take M, V, and apply M to get world
coordinates, V * M to get view coordinates.

For normals M is applied to (nx,ny,nx,0). For vertices, M is applied
to (vx,vy,vz,1)

For normals V * M is applied to (nx,ny,nx,0). For vertices, V * M is applied
to (vx,vy,vz,1)

The tessellation control shader takes the vertex output data for a
vertex of the primitive and copies the material, normal, and so on, to its outputs. It also uses
the vertex data for *all* the vertices in the primitive (triangle or
quad really) to determine how much tessellation to do. (The number of
output triangles is approximately 3/4 of inner tessellation level
squared, for triangle primitives).

The tessellation control shader may cause a primitive to be dropped by
returning 0 as the inner tessellation level (for example if it is
outside the culling region or back-facing).

The tessellation evaluation shader takes the tessellation evaluation
shader outputs for the primitive's vertices and Barycentric
coordinates (in gl_TessCoord.xyz); it is responsible for producing the
same outputs as a vertex shader would if there were no tessellation
shader invoked. Hence the tesselllation evaulation shader must also
utilize G and P, and it should utilize gl_TessCoord.xyz and the
primitives' vertices' data to determine world coordinates, view
normals, and portal projected coordinates.

## Fragment shader

The fragment shader is invoked to produce, for each possibly-visible
pixel, a set of realistic render buffer inputs.

The output then must include a depth, as the data for the pixel in the
fragment is only written if the depth is less than that stored in the
depth buffer.

A pixel may be discarded if there is, for example, a stencil texture supplied
to the fragment shader that has a hole at the fragment u, v.

The outputs from the fragment shader are required to be standard, so
that a realistic shading engine can render from the realistic buffer
values to a framebuffer.

One thought for the fragment shader is that it may be run a plurality
of times, to permit some form of 'x-ray' vision to objects/scenes. The
output is still the realistic render buffer inputs. Perhaps this
actually requires a blend of two sets of realistic render buffers.

## Realistic shader

The realistic shader is provided by a portal, and it maps from the
fragment shader outputs, using a global illumination model,
atmospheric effects as desired, pixel-by-pixel to the output
framebuffer. It provides also for blending the output of one realistic
shader with previous frame buffer contents, to provide (for example)
for translucent HUD elements.

# Desires

We want to be able to achieve a drone-style follow-a-bus flyover on a
topographic 3D view of a city, where there are some 3D buildings with
simple textures (and a light map for direct lighting shadows?)

Buses may be ghostlike - which can be performed using pixel culling
(does that work in stereo?), or through transparency (?)


#version 410 core

layout(vertices = 3) out;
in vec4 v_color[];
out vec4 t_color[];

void main(void)
{
 gl_TessLevelOuter[0] = 6.0;
 gl_TessLevelOuter[1] = 6.0;
 gl_TessLevelOuter[2] = 6.0;

 gl_TessLevelInner[0] = 12.0;
 gl_TessLevelInner[1] = 12.0;

 gl_out[gl_InvocationID].gl_Position = gl_in[gl_InvocationID].gl_Position;
 t_color[gl_InvocationID] = v_color[gl_InvocationID];
}

#version 410 core

layout(vertices = 3) out;
in vec4 v_color[];
in vec4 v_normal[];
out vec4 t_color[];
out vec4 t_normal[];

// IL -> # tris
// 1  ->  1                 = 1
// 3  ->  1+3*2             = 1+6   = 7
// 5  ->  1+3*2+3*4         = 7+12  = 19
// 7  ->  1+3*2+3*4+3*6     = 19+18 = 37
// 9  ->  1+3*2+3*4+3*6+3*8 = 37+24 = 61
// 11 ->  1+...+3*10        = 61+30 = 91

// 2  ->   3                 = 3
// 4  ->   3 + 3*3           = 3+9   = 12
// 6  ->   3+3*3+3*5         = 12+15 = 27
// 8  ->   3+3*3+3*5+3*7     = 27+21 = 48
// 10 ->   3+3*3+3*5+3*7+3*9 = 48+27 = 75
// 12 ->   3+...+3*11        = 75+33 = 108
void main(void)
{
 gl_TessLevelOuter[0] = 12.0; // should really match the inner level
 gl_TessLevelOuter[1] = 12.0;
 gl_TessLevelOuter[2] = 12.0;

 gl_TessLevelInner[0] = 12.0; // means split the outside into 12, then 10, then 8 etc inwards
// 12 is VERY high...

 gl_out[gl_InvocationID].gl_Position = gl_in[gl_InvocationID].gl_Position;
 t_color[gl_InvocationID] = v_color[gl_InvocationID];
 t_normal[gl_InvocationID] = v_normal[gl_InvocationID];
}

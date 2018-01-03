#version 410 core
 
in vec4 t_color[];
out vec4 v_color;

layout(triangles, equal_spacing, ccw) in;
 
void main()
{ 
    v_color = ( (t_color[0] * gl_TessCoord.x) +
                (t_color[1] * gl_TessCoord.y) +
                (t_color[2] * gl_TessCoord.z) 
        );
    //v_color = t_color[2];
 vec4 p4 = ( (gl_TessCoord.x * gl_in[0].gl_Position) +
                  (gl_TessCoord.y * gl_in[1].gl_Position) +
                  (gl_TessCoord.z * gl_in[2].gl_Position)
     );
 vec3 p3 = vec3(p4.x, p4.y, p4.z);
 float l = sqrt(dot(p3,p3));
 vec3 n3 = p3 * l;
 gl_Position = vec4( n3.x, n3.y, n3.z, p4.w );
}

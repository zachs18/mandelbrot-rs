#version 460
layout (location=0) in vec2 vertex;
uniform vec2 center;
// uniform float rot;
uniform vec2 scale;

out vec2 pos;

// const vec2 quadVertices[4] = { vec2(-1.0, -1.0), vec2(1.0, -1.0), vec2(-1.0, 1.0), vec2(1.0, 1.0) };
void main() {
    vec2 vert = vertex;
    // mat2 rotmat = {{cos(rot), -sin(rot)},{sin(rot),cos(rot)}};
    // vert *= rotmat;
    vert += center;
    vert *= scale;
    // gl_Position = vec4(vert.x, vert.y, 0, 1);

    // gl_Position = vec4(quadVertices[gl_VertexID], 0.0, 1.0);
    gl_Position = vec4(vertex, 0.0, 1.0);
    pos = vert;
}

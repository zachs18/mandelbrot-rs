#version 460
layout (location=0) in vec2 vertex;
uniform vec2 pos;
uniform float rot;
uniform vec2 scale;
void main() {
    vec2 vert = vertex;
    mat2 rotmat = {{cos(rot), -sin(rot)},{sin(rot),cos(rot)}};
    vert *= rotmat;
    vert += pos;
    vert /= scale;
    gl_Position = vec4(vert.x, vert.y, 0, 1);
}

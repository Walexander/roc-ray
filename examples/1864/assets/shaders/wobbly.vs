#version 100
// vim:filetype=glsl
precision mediump float;

attribute vec3 vertexPosition;
attribute vec2 vertexTexCoord;
attribute vec4 vertexColor;

uniform mat4 mvp;
uniform float time;

varying vec2 fragTexCoord;
varying vec4 fragColor;

void main()
{
    vec3 pos = vertexPosition;

    // Subtle wobble based on y-position
    float wobble = sin(time * 3.0 + pos.y * 0.08) * 2.0;

    pos.x += wobble;

    fragTexCoord = vertexTexCoord;
    fragColor = vertexColor;

    gl_Position = mvp * vec4(pos, 1.0);
}

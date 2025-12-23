#version 100
precision mediump float;

attribute vec3 vertexPosition;
attribute vec2 vertexTexCoord;
attribute vec4 vertexColor;

uniform mat4 mvp;

varying vec2 fragTexCoord;
varying vec4 fragColor;

uniform vec2 u_offset;
void main()
{
    vec3 pos = vertexPosition;
    pos.xy += u_offset;   // <-- TEST UNIFORM

    fragTexCoord = vertexTexCoord;
    fragColor = vertexColor;

    gl_Position = mvp * vec4(pos, 1.0);
}


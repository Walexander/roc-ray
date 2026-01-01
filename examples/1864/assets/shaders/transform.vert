#version 100
// vim:filetype=glsl
precision mediump float;
attribute vec3 vertexPosition;
attribute vec2 vertexTexCoord;
attribute vec4 vertexColor;

varying vec2 fragTexCoord;
varying vec4 fragColor;
varying vec2 fragPos;
uniform mat4 mvp;
uniform mat4 u_model;

void main()
{
    fragTexCoord = vertexTexCoord;
    fragColor = vertexColor;
    fragPos = vertexPosition.xy;
    gl_Position = u_model * mvp * vec4(vertexPosition, 1.0);
    // gl_Position = mvp * vec4(vertexPosition.xy / 100.0, 0.0, 1.0);
    /* Draw a single point at the transformed origin */
    // gl_Position = mvp * vec4(0.0, 0.0, 0.0, 1.0);
    // gl_PointSize = 10.0;
}

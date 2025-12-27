#version 100
// vim:filetype=glsl
precision mediump float;

varying vec2 fragTexCoord;
varying vec4 fragColor;

uniform sampler2D texture0;

void main()
{
    gl_FragColor = texture2D(texture0, fragTexCoord) * fragColor;
}


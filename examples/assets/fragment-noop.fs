#version 100
precision mediump float;
varying vec2 fragTexCoord;
varying vec4 fragColor;
uniform sampler2D texture0;
uniform vec4 colDiffuse;
uniform float time;
uniform float frequency;
uniform float amplitude;

void main()
{
    gl_FragColor = fragColor;
}

#version 100
precision mediump float;

varying vec2 fragTexCoord;
varying vec4 fragColor;

uniform sampler2D texture0;

/* 1.0 / fogRenderTextureSize */
uniform vec2 texelSize;

/* tweak: 1.0–2.5 is typical */
uniform float blurStrength;

void main()
{
    vec2 t = texelSize * blurStrength;

    vec4 color = vec4(0.0);

    color += texture2D(texture0, fragTexCoord + vec2(-t.x, -t.y)) * 0.0625;
    color += texture2D(texture0, fragTexCoord + vec2( 0.0, -t.y)) * 0.125;
    color += texture2D(texture0, fragTexCoord + vec2( t.x, -t.y)) * 0.0625;

    color += texture2D(texture0, fragTexCoord + vec2(-t.x,  0.0)) * 0.125;
    color += texture2D(texture0, fragTexCoord)                  * 0.25;
    color += texture2D(texture0, fragTexCoord + vec2( t.x,  0.0)) * 0.125;

    color += texture2D(texture0, fragTexCoord + vec2(-t.x,  t.y)) * 0.0625;
    color += texture2D(texture0, fragTexCoord + vec2( 0.0,  t.y)) * 0.125;
    color += texture2D(texture0, fragTexCoord + vec2( t.x,  t.y)) * 0.0625;

    gl_FragColor = color * fragColor;
}

#version 100
// vim:filetype=glsl
precision mediump float;
varying vec2 fragTexCoord;
varying vec4 fragColor;
uniform sampler2D texture0;
uniform vec4 colDiffuse;
uniform float time;
const float frequency = 1.0;

const float amplitude = 1.00;

void main()
{
    vec4 texelColor = texture2D(texture0, fragTexCoord);
    float f = frequency;
    vec2 uv = fragTexCoord - 0.5;
    float pct = 0.0;
    pct = distance(gl_FragCoord.xy, vec2(0.5));
    float dist = length(uv);
    float ripple = sin(dist * frequency * 1.0 - time * 4.0) * amplitude;
    vec3 color = vec3(pct * texelColor);
    // vec2 distortedUV = fragTexCoord + normalize(uv) * ripple * 0.001; // vec2(fragTexCoord.x + waveX, fragTexCoord.y + waveY);
    gl_FragColor = fragColor; //texture2D(texture0, distortedUV);
}

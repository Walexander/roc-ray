#version 100
// vim:filetype=glsl
precision mediump float;

uniform sampler2D texture0;
uniform float progress;     // 0.0 → 1.0
const vec4 fillColor = vec4(0.0, 0.0, 0.0, 0.8);     // the color that fills the circle

varying vec2 fragTexCoord;
varying vec4 fragColor;

const float PI = 3.14159265359;

void main() {

    vec4 base = texture2D(texture0, fragTexCoord) * fragColor;

    // Centered coords for circle mask
    vec2 p = fragTexCoord * 2.0 - 1.0;
    float radius = length(p);
    float circleMask = step(radius, 1.0);

    // Bottom → top fill
    // fragTexCoord.y = 0 at bottom, 1 at top
    float fillMask = smoothstep(progress * 0.01, progress * 0.01, fragTexCoord.y) * circleMask;

    // Invert colors where filled
    vec3 inverted = vec3(1.0) - base.rgb;
    vec3 mixed = mix(base.rgb, inverted, fillMask);

    // Optional color tint on fill
    // mixed = mix(mixed, fillColor.rgb, fillMask * fillColor.a);

    gl_FragColor = vec4(mixed, base.a);
}

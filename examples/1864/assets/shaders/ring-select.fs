#version 100
// vim:filetype=glsl
precision mediump float;

varying vec2 fragTexCoord;
varying vec2 fragPos;

uniform float time;        // seconds
uniform float duration;    // total animation time
uniform float thickness;   // ring thickness (e.g. 0.05)
uniform vec2 center;

float saturate(float x) {
    return clamp(x, 0.0, 1.0);
}
void main() {

    vec2 fromCenter = fragPos - center;

    float radius = sqrt(dot(fromCenter, fromCenter));

    float outer = clamp(64.0 * fract(1.0 - (time / duration)), 16.0, 64.0);

    float inner = outer - 8.0;

    float pulse = saturate(radius - inner) * saturate(outer - radius);
    vec4 pulseColor = vec4(0.95, 0.95, 0.9, 1.0);
    vec4 background = vec4(0.0, 0.0, 0.0, 0.0);

    vec4 fragColor = mix(background, pulseColor, pulse);
    // fragColor.rgb = sqrt(fragColor.rgb);
    // Center UVs at (0.5, 0.5)
    // vec2 p = fragTexCoord - vec2(0.5);

    // float dist = length(p);

    // // Normalize distance so radius 1.0 reaches quad edge
    // dist /= 0.5;

    // // Animation progress 1 → 0 (shrinking)
    // float t = clamp(time / duration, 0.0, 1.0);
    // float outerRadius = mix(1.0, 0.0, t);

    // // float outerRadius = 0.7 + 0.1 * sin(time * 6.0);

    // float innerRadius = outerRadius - thickness;

    // // Ring mask
    // float ring =
    //     step(innerRadius, dist) *
    //     (1.0 - step(outerRadius, dist));

    // // Soft edge (optional, looks nicer)
    // ring *= smoothstep(outerRadius, outerRadius - 0.02, dist);

    gl_FragColor = fragColor;
    // gl_FragColor = mix(fragColor, vec4(color.rgb, color.a * ring), 0.0);
}

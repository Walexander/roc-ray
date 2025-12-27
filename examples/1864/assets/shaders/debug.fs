#version 100
// vim:filetype=glsl
precision mediump float;

uniform float progress;   // 0.0 = empty, 1.0 = full

uniform sampler2D texture0;
varying vec2 fragTexCoord;
varying vec4 fragColor;
varying vec2 fragPos;
uniform vec2 bounds;

// const vec4 FILL_COLOR = vec4(0.2, 0.6, 1.0, 1.0); // blue

void main()
{
    // vec3 tc = fragPos * vec2(2.0) - vec2(1.0);
    vec2 scaled = fragPos * 0.01;
    vec3 posColor = vec3(
        abs(fract(fragPos.x)),
        abs(fract(fragPos.y)),
        1.0
    );

    // float threshold = mix(-50.0, 50.0, progress);
    float threshold = mix(bounds.x, bounds.y, progress);
    float above = step(threshold, fragPos.y);

    vec3 progressColor = mix(
        vec3(1.0, 0.0, 0.0),
        vec3(0.0, 1.0, 0.0),
        above
    );
    vec3 finalColor = mix(vec3(fragColor.xy, 1.0), progressColor, 0.6);
    gl_FragColor = vec4(finalColor, 0.95);
}

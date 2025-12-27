#version 100
// vim:filetype=glsl
precision mediump float;

uniform float progress;   // 0.0 = empty, 1.0 = full

uniform sampler2D texture0;
varying vec2 fragTexCoord;
varying vec4 fragColor;

const vec4 fillColor = vec4(0.2, 0.6, 1.0, 1.0); // blue
const vec4 bgColor = vec4(0.9, 1.5, 1.0, 1.0); // blue

void main()
{

    // fragTexCoord assumed to go from 0.0 to 1.0 across the quad
    float thresholdY = 1.0 - progress; // fill from bottom

    vec2 tc = fragTexCoord;
    // Compute horizontal line mask (small band at threshold)
    float lineThickness = 0.01;
    float lineMask = step(thresholdY - lineThickness, tc.y) *
                     step(tc.y, thresholdY + lineThickness);

    // Compute filled area below threshold
    float fillMask = step(fragTexCoord.y, thresholdY);

    // Color assignment
    vec4 color = bgColor;                  // start with background
    color = mix(color, fillColor, fillMask);  // fill area below threshold
    color = mix(color, vec4(1.0), lineMask);  // draw threshold line (white)

    gl_FragColor = color;

}

#version 100
precision mediump float;

varying vec2 fragTexCoord;

uniform float u_time;      // seconds since explosion start
// uniform vec3  u_color;     // base explosion color (orange/yellow)
uniform float u_duration;  // total lifetime (e.g. 1.0)

const vec3 u_color = vec3(1.0, 0.25, 0.1);

float hash(vec2 p) {
    return fract(sin(dot(p, vec2(127.1, 311.7))) * 43758.5453123);
}

float noise(vec2 p) {
    vec2 i = floor(p);
    vec2 f = fract(p);
    f = f * f * (3.0 - 2.0 * f);

    float a = hash(i);
    float b = hash(i + vec2(1.0, 0.0));
    float c = hash(i + vec2(0.0, 1.0));
    float d = hash(i + vec2(1.0, 1.0));

    return mix(mix(a, b, f.x), mix(c, d, f.x), f.y);
}

// void main() {
//     vec2 uv = fragTexCoord * 2.0 - 1.0;
//     float dist = length(uv);

//     float t = clamp(u_time / u_duration, 0.0, 1.0);

//     // Explosion radius
//     float radius = t * 1.25;

//     // Stronger turbulence
//     float n = noise(uv * 8.0 + u_time * 5.0);
//     float warpedDist = dist + n * 0.18;

//     // Fireball body
//     float body = smoothstep(radius + 0.25, radius, warpedDist);

//     // Shock ring (thin + aggressive)
//     float ring = smoothstep(radius + 0.02, radius - 0.02, warpedDist);

//     // White-hot core
//     float core = smoothstep(0.35, 0.0, dist);
//     core = pow(core, 1.5); // tighten it

//     // Fade driven by time, but not killing energy early
//     float fade = smoothstep(1.0, 0.7, t);

//     float alpha = max(max(body, ring), core) * fade;

//     // === COLOR ===

//     vec3 white  = vec3(3.0);                // overbright
//     vec3 yellow = vec3(2.5, 2.0, 0.5);
//     vec3 orange = u_color * 2.2;
//     vec3 smoke  = vec3(0.15);

//     vec3 color =
//         white  * core +
//         yellow * ring +
//         orange * body * (1.0 - t) +
//         smoke  * body * t * t;

//     gl_FragColor = vec4(color, alpha);
// }

void main_particle() {
    vec2 uv = fragTexCoord * 2.0 - 1.0;
    float dist = length(uv);
    float angle = atan(uv.y, uv.x);

    float t = clamp(u_time / u_duration, 0.0, 1.0);

    // === PARTICLE SETUP ===
    const float PARTICLE_COUNT = 36.0;

    // Map angle → particle index
    float id = floor((angle + 3.14159) / (2.0 * 3.14159) * PARTICLE_COUNT);
    float seed = id / PARTICLE_COUNT;

    // Randomized parameters per particle
    float speed = mix(0.6, 1.4, hash(vec2(seed, 1.3)));
    float size  = mix(0.04, 0.08, hash(vec2(seed, 2.7)));
    float jitter = hash(vec2(seed, 9.1)) * 0.3;

    // Particle radial position
    float r = t * speed + sin(t * 8.0 + seed * 20.0) * 0.03;
    r += noise(uv * 6.0 + t * 5.0) * 0.05;

    // Distance to this particle
    float d = abs(dist - r);

    // Particle shape
    float particle = smoothstep(size, 0.0, d);

    // Fade over time
    particle *= smoothstep(1.0, 0.4, t);

    // === COLOR ===
    vec3 hot   = vec3(3.0, 2.0, 0.6);
    vec3 fire  = u_color * 2.5;
    vec3 smoke = vec3(0.15);

    vec3 color =
        hot * particle * (1.0 - t) +
        fire * particle * t +
        smoke * particle * t * t;

    gl_FragColor = vec4(color, particle);
}

void main() {
    vec2 uv = fragTexCoord * 2.0 - 1.0;
    float dist = length(uv);
    float angle = atan(uv.y, uv.x);

    float t = clamp(u_time / u_duration, 0.0, 1.0);

    // === PARTICLES ===
    const float PARTICLE_COUNT = 28.0;

    float id = floor((angle + 3.14159) / (2.0 * 3.14159) * PARTICLE_COUNT);
    float seed = id / PARTICLE_COUNT;

    // Per-particle randomness
    float speed   = mix(1.2, 2.0, hash(vec2(seed, 1.1)));
    float gravity = mix(1.5, 2.5, hash(vec2(seed, 2.2)));
    float size    = mix(0.04, 0.08, hash(vec2(seed, 3.3)));

    // === GRAVITY ARC ===
    // r(t) = v*t - g*t²
    float r = speed * t - gravity * t * t;

    // Kill particles that fall back past center
    r = max(r, 0.0);

    // Slight angular wobble (chaos)
    float wobble = sin(t * 10.0 + seed * 20.0) * 0.03;

    float d = abs(dist - (r + wobble));

    float particle = smoothstep(size, 0.0, d);

    // Fade near end of life
    particle *= smoothstep(1.0, 0.6, t);

    // === COLOR ===
    vec3 white  = vec3(3.0);
    vec3 fire   = u_color * 2.8;
    vec3 smoke  = vec3(0.12);

    vec3 color =
        white * particle * (1.0 - t) +
        fire  * particle * t +
        smoke * particle * t * t;

    gl_FragColor = vec4(color, particle);
}

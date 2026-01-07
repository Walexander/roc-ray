#version 100
precision mediump float;

uniform float u_progress;   // 0 → 1
uniform float u_scale;

varying vec2 fragTexCoord;

/* ---------------- Utility ---------------- */

float gyroid(vec3 p)
{
    return dot(cos(p), sin(p.yzx));
}

float fbm(vec3 p)
{
    float result = 0.0;
    float a = 0.5;

    for (int i = 0; i < 8; i++)
    {
        p.z += result * 0.1;
        result += abs(gyroid(p / a) * a);
        a /= 1.7;
    }

    return result;
}

/* ---------------- Main ---------------- */

void main()
{
    // Sprite-local UVs centered at 0
    vec2 uv = fragTexCoord - 0.5;
    uv *= u_scale;

    float t = clamp(u_progress, 0.0, 1.0);

    // Animation curves
    float growth = pow(t, 0.25);
    float fade   = 1.0 - pow(t, 6.0);
    float burn   = 1.0 - pow(t, 0.4);
    float speed  = pow(t, 0.4);

    // Ray direction (local)
    vec3 ray = normalize(vec3(uv, 0.2 + t));
    ray.z += speed * 2.0;

    float noise = fbm(ray);

    // Normal estimation (local scale)
    vec3 e = vec3(0.1, 0.1, 0.0);

    float nx = fbm(ray + e.xzz);
    float ny = fbm(ray + e.zyz);

    vec3 normal = normalize(noise - vec3(nx, ny, 1.0));

    // Fire palette
    vec3 color = 0.3 + 1.2 *
        cos(vec3(1.0, 2.0, 3.0) * 5.5 + normal.y);

    // Smoke & burn
    float smoke = noise - 2.0 * burn;
    float shade = normal.y * 0.5 + 0.5;

    color = mix(color, vec3(smoke * shade),
                smoothstep(0.0, 0.15, smoke));

    // Explosion shape
    float radius = 0.35 * noise * growth;
    float shape = smoothstep(0.02, 0.0, length(uv) - radius);

    float fireAlpha = shape * (1.0 - t);

    float smokeAlpha = shape * smoothstep(0.4, 1.0, t) * 0.35;

    float alpha = (fireAlpha + smokeAlpha) * fade;
    color *= alpha;

    gl_FragColor = vec4(color, alpha);
}

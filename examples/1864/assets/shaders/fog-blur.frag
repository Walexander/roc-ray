#version 100

precision mediump float;

varying vec2 fragTexCoord;
varying vec4 fragColor;

uniform sampler2D texture0;
uniform vec2 texelSize;   // (1/width, 1/height)
uniform float radius;     // blur radius in pixels

void main()
{
    vec4 center = texture2D(texture0, fragTexCoord);

    float alphaSum = 0.0;
    float count = 0.0;

    for (float x = -1.0; x <= 1.0; x++)
    {
        for (float y = -1.0; y <= 1.0; y++)
        {
            vec2 offset = vec2(x, y) * texelSize * radius;
            float a = texture2D(texture0, fragTexCoord + offset).a;
            alphaSum += a;
            count += 1.0;
        }
    }

    float alpha = 4.*texture2D( texture0, fragTexCoord ).a;
    alpha -= texture2D( texture0, fragTexCoord + vec2( texelSize.x, 0.0 ) ).a;
    alpha -= texture2D( texture0, fragTexCoord + vec2( -texelSize.x, 0.0 ) ).a;
    alpha -= texture2D( texture0, fragTexCoord + vec2( 0.0, texelSize.y ) ).a;
    alpha -= texture2D( texture0, fragTexCoord + vec2( 0.0, -texelSize.y ) ).a;
    vec4 resultCol = vec4(0.2, 0.6, 1.0, alpha);
    gl_FragColor = resultCol;
}

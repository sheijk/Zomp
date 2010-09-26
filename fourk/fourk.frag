
uniform float time;
const float hitThreshhold = 0.00001;

vec3 displace(vec3 pos)
{
    float dz = abs(sin( pos.x * 4.0 )) + abs( sin(pos.z * 4.0) );
    dz *= 0.5;
    dz = 1. - pow(dz, 4.);
    /* dz = 1. - dz; */
    /* float dz = 0.5 * (sin(pos.x * 4.0) + sin(pos.z * 4.0)); */
    /* dz = dz * dz; */
    /* dz = 1.0 - dz; */

    /* float centerDist = length( pos.xz - vec2( 1., -5. ) ); */
    /* vec3 distortedPos = pos + vec3( 0., 0.1 * , 0. ); */
    /* pos.y += smoothstep( 4.0, 1.0, centerDist ) * 0.2 * dz; */
    pos.y += smoothstep( -1.0, -1.1, pos.z ) * 0.2 * dz;
    return pos;
}

void mindistSphere(
    vec3 spherePos, float sphereSize, bool deform,
    vec3 pos,
    inout float mindist, inout bool hit, inout vec3 normal, inout vec3 color)
{
    vec3 localSpherePos = pos - spherePos;
    if( deform )
    {
        const float frequency = 100.;
        const float scale = 0.1;
        float d = sin(localSpherePos.y * frequency);
        /* localSpherePos *= 0.9 + 0.1 * d; */
        localSpherePos.xz *= (1.-scale) + scale * d;
    }
    float dist2sphere = max( 0.0, sqrt(dot(localSpherePos, localSpherePos)) - sphereSize );

    if( dist2sphere < mindist )
    {
        mindist = dist2sphere;
        if( dist2sphere < hitThreshhold )
        {
            hit = true;
            normal = normalize(localSpherePos);
            /* color = vec3( 0.3, 0.3, 0.3 ) * 2.; */
            color = vec3( 1., 1., 1. );
        }
    }
}

void mindistPlane(
    vec3 planeDir, float planeDist,
    vec3 pos,
    inout float mindist, inout bool hit, inout vec3 normal, inout vec3 color)
{
    /* vec3 distortedPos = displace( pos ); */
    vec3 distortedPos = pos;
    float dist2plane = max( 0.0, dot(distortedPos - planeDist*planeDir, planeDir) );

    if( dist2plane < mindist )
    {
        mindist = dist2plane;
        if( dist2plane < hitThreshhold )
        {
            hit = true;
            float d = 0.001;
            /* vec3 du = displace( vec3(pos.x + d, pos.y, pos.z) ) - displace( vec3(pos.x - d, pos.y, pos.z) ); */
            /* vec3 dv = displace( vec3(pos.x, pos.y, pos.z + d) ) - displace( vec3(pos.x, pos.y, pos.z - d) ); */
            /* normal = cross( normalize( dv ), normalize( du ) ); */
            normal = planeDir;

            /// show distance from origin
            /* float c = step( 1.0, mod( centerDist, 2.0 ) ); */
            /* color = vec3( c, c, c ); */
            /* color = vec3( 1., 1., 1. ); */

            bool colSquare = step( 0.5, mod( pos.x, 1.0 ) ) == step( 0.5, mod( pos.z, 1.0 ) );
            if( colSquare )
                color = vec3( 1., 0.6, 0.0 );
            else
                color = vec3( 0.4, 0.15, 0.0 );
        }
    }
}

float mindist(vec3 pos, out bool hit2, out vec3 normal, inout vec3 color)
{
    float mindist = 1000.0;
    bool hit = false;
    float t = 1.5 * time;
    /* mindistSphere(vec3(sin(t),0.5 + 0.1*sin(10.*time),-2.3+cos(t)), 0.2, pos, mindist, hit, normal, color); */
    mindistSphere(vec3(0.5, -1., 0.5), 0.2, false, vec3(mod(pos.x,1.0), pos.y, mod(pos.z,1.0)), mindist, hit, normal, color);
    mindistSphere(vec3(0.,-0.3,-2.51), 0.5, true, pos, mindist, hit, normal, color);
    mindistPlane(vec3(0.,1.,0.), -1., pos, mindist, hit, normal, color);
    hit2 = hit;
    return mindist;
}

vec4 sky(vec3 dir)
{
    float h = dot(dir, vec3(0.,1.,0.));
    return (1.0-h) * vec4(0.0,0.,0.4,1.) + h * vec4(0.6,0.6,1.0,1.);
}

vec4 visNormal(vec3 norm)
{
    return vec4(norm*0.5+0.5, 1.0);
}

vec4 visDepth(vec3 pos)
{
    return vec4(vec3(( -1.0 - pos.z ) / 1.0), 1.0);
}

vec4 trace(vec3 pos, vec3 dir)
{
    float lastDist = 10000.0;
    int depth = 0;
    vec3 normal;
    /* vec3 color = vec3( 0., 0., 0. ); */
    vec3 color = vec3( 0., 0., 0. );
    const vec3 ambientColor = vec3( 0.05, 0.05, 0.1 );
    while( true ) {
        bool hit = false;
        float d = mindist(pos, hit, normal, color);

        if( hit ) {
            if( pos.z >= -1. ) {
                return vec4(1.,1.,0.,1.);
            }
            /* return visDepth(pos); */
            /* return visNormal(normal); */

            vec3 baseColor = vec3(1., 0.5, 0.0);

            float vis = 0.0;
            float maxvis = 0.0;
            vec3 dummyNormal, dummyColor;
            for( int i = 0; i < 4; ++i )
            {
                vec3 vispos = pos + normal * 0.5 * float(i);
                float d = mindist( vispos, hit, dummyNormal, dummyColor );
                float occl = clamp( d, 0.0, 1.0 );
                vis += occl / float(i+1);
                maxvis += 1.0 / float(i+1);
            }

            const int lightCount = 3;
            vec3 lightPositions[lightCount];
            lightPositions[0] = vec3( 1., 1., -1. );
            lightPositions[1] = vec3( -1., 1., -1. );
            /* lightPositions[2] = vec3( 0.0, -0.5, -2. ); */
            lightPositions[2] = vec3( 0., 2., -3. );

            vec3 lightColors[lightCount];
            lightColors[0] = vec3( 0.3, 0.6, 1.0 ) * 1.;
            lightColors[1] = vec3( 1.0, 0.6, 0.3 ) * 1.;
            lightColors[2] = vec3( 1., 1., 1. ) * 1.;

            vec3 totalColor = vec3( 0., 0., 0. );
            for( int i = 0; i < lightCount; ++i )
            {
                vec3 lightDir = normalize( lightPositions[i] - pos );
                float dist = length( lightPositions[i] - pos );
                float att = max( 1., dist * dist );
                /* totalColor += lightColors[i] * color / att; */
                totalColor += lightColors[i] * color * max(0., dot(normal, lightDir)) / att;
            }

            /* return vec4(min(totalColor * vis, ambientColor), 1.0); */
            /* return vec4(totalColor * vis, 1.0); */
            return vec4(totalColor * vis + ambientColor, 1.0);

            /* vec3 lightPos = vec3( 1., 1., -1. ); */
            /* vec3 lightDir = normalize( lightPos - pos ); */
            /* return vec4(1., 0.5, 0.0, 1.0) * dot(normal, lightDir) * vis; */
        }
        /* if( d > lastDist ) */
        /*     return vec4(float(depth)/5.0, 0., 0., 0.); */

        if ( pos.z < -10.0 )
            return sky(dir);

        if( depth > 200 )
            return vec4(1.,0.,1.,1.);
            /* return sky(dir); */

        /* if( d < 0.1 ) */
        /*     return vec4(1., 0.5, 0., 1.); */

        pos += max(d, 0.01) * dir;
        ++depth;
    }
}

void main(void)
{
    /* gl_FragColor = vec4(1., 0.4, 0., 1.); */
    /* gl_FragColor = vec4(texCoord.x, texCoord.y, 0.0, 1.0); */
    vec3 d = vec3( texCoord * 2.0 - 1.0, -1.0 );
    gl_FragColor = trace( d, normalize(d) );
    /* gl_FragColor = trace( vec3(texCoord * 2.0 - 1.0, 1.0), vec3(texCoord, -1.0) ); */
    /* gl_FragColor = trace( vec3( 0.,0.,0. ), vec3( texCoord, -1.0 ) ); */
}


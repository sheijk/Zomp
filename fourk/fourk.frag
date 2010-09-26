
uniform float time;
const float hitThreshhold = 0.00001;

void mindistSphere(vec3 spherePos, float sphereSize, vec3 pos, inout float mindist, inout bool hit, inout vec3 normal)
{
    vec3 localSpherePos = pos - spherePos;
    float dist2sphere = max( 0.0, sqrt(dot(localSpherePos, localSpherePos)) - sphereSize );

    if( dist2sphere < mindist )
    {
        mindist = dist2sphere;
        if( dist2sphere < hitThreshhold )
        {
            hit = true;
            normal = normalize(localSpherePos);
        }
    }
}

void mindistPlane(vec3 planeDir, float planeDist, vec3 pos, inout float mindist, inout bool hit, inout vec3 normal)
{
    float dist2plane = max( 0.0, dot(pos - planeDist*planeDir, planeDir) );

    if( dist2plane < mindist )
    {
        mindist = dist2plane;
        if( dist2plane < hitThreshhold )
        {
            hit = true;
            normal = planeDir;
        }
    }
}

float mindist(vec3 pos, out bool hit2, out vec3 normal)
{
    float mindist = 1000.0;
    bool hit = false;
    float t = 1.5 * time;
    mindistSphere(vec3(sin(t),0.5 + 0.1*sin(10.*time),-2.3+cos(t)), 0.2, pos, mindist, hit, normal);
    mindistSphere(vec3(0.,-0.3,-2.51), 0.5, pos, mindist, hit, normal);
    mindistPlane(vec3(0.,1.,0.), -1., pos, mindist, hit, normal);
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
    while( true ) {
        bool hit = false;
        float d = mindist(pos, hit, normal);

        if( hit ) {
            if( pos.z >= -1. ) {
                return vec4(1.,1.,0.,1.);
            }
            /* return visDepth(pos); */
            /* return visNormal(normal); */

            vec3 baseColor = vec3(1., 0.5, 0.0);

            float vis = 0.0;
            float maxvis = 0.0;
            vec3 dummyNormal;
            for( int i = 0; i < 4; ++i )
            {
                vec3 vispos = pos + normal * 0.5 * float(i);
                float d = mindist( vispos, hit, dummyNormal );
                float occl = clamp( d, 0.0, 1.0 );
                vis += occl / float(i+1);
                maxvis += 1.0 / float(i+1);
            }

            vec3 lightPositions[2];
            lightPositions[0] = vec3( 1., 1., -1. );
            lightPositions[1] = vec3( -1., 1., -1. );

            vec3 lightColors[2];
            lightColors[0] = vec3( 0.6, 0.8, 1.0 );
            lightColors[1] = vec3( 1.0, 0.8, 0.6 );

            vec3 color = vec3( 0.2, 0.2, 0.2 );
            for( int i = 0; i < 2; ++i )
            {
                vec3 lightDir = normalize( lightPositions[i] - pos );
                color += lightColors[i] * max(0., dot(normal, lightDir));
            }

            return vec4(color,1.0) * vis;

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

        pos += max(d, 0.001) * dir;
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


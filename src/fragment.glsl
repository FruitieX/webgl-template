precision highp float;

// time variable (seconds)
uniform float a;
// resolution (1920.0, 1080.0)
uniform vec2 b;

// bass
uniform float c;
// treble
uniform float d;
// accumulated bass
uniform float e;
// frequency of lead synth
uniform float f;

float PI = 3.14;

float smin( float a, float b, float k ) {
  return -log(exp( -k*a ) + exp( -k*b ))/k;
}

float sdSphere(vec3 p, float s) {
  return length(p)-s;
}

float udBox( vec3 p, vec3 b ) {
  return length(max(abs(p)-b,.0));
}
// TODO: test me
vec2 opS(vec2 d1, vec2 d2) {
  return (d1.x<-d2.x) ? d2 : d1;
}

vec2 opBlend( vec2 d1, vec2 d2, float k ) {
    return vec2(
      smin( d1.x, d2.x, k ),
      smin( d1.y, d2.y, k )
    );
}
vec2 opU(vec2 d1, vec2 d2) {
  return (d1.x<d2.x) ? d1 : d2;
}

vec3 opRep(vec3 p, vec3 c) {
  return mod(p,c)-.5*c;
}

// Rotate around a coordinate axis (i.e. in a plane perpendicular to that axis) by angle <a>.
// Read like this: R(p.xz, a) rotates "x towards z".
// This is fast if <a> is a compile-time constant and slower (but still practical) if not.
vec2 pR(vec2 p, float a) {
	return cos(a)*p + sin(a)*vec2(p.y, -p.x);
}

vec3 opTwist(vec3 p) {
  float  c = cos(p.y);
  float  s = sin(p.y);
  mat2   m = mat2(c,-s,s,c);
  return vec3(m*p.xz,p.y);
}
float fCapsule(vec3 p, float r, float c) {
	return mix(length(p.xz) - r, length(vec3(p.x, abs(p.y) - c, p.z)) - r, step(c, abs(p.y)));
}


// SCENES

vec2 scene1(vec3 pos) {
  // Sphere
  return vec2(sdSphere(
    pos,
  .5), 1.);
}

vec2 map(in vec3 pos, in vec3 origin) {
  vec2 res = vec2(.0);

  float transitionTime = 10.;
  float end0 = 20.;
  float end1 = 34.;
  float end2 = 50.;
  float end3 = 70.;

  // Uncomment when debugging single scene
  return scene1(pos);

  /* ---------- SCENES --------- */

  // first scene
  if (a < end0 + transitionTime) {
    res = scene1(pos);
  }
  return res;
}

vec2 castRay(in vec3 ro, in vec3 rd) {
  const int maxIterations = 64;
  float tmin = .02;
  float tmax = 50.;

  float t = tmin;
  float m = -1.;
  for( int i=0; i<maxIterations; i++ ) {
    float precis = .000001*t;
    vec2 res = map( ro+rd*t, ro );
    if( res.x<precis || t>tmax ) break;
    t += res.x;
    m = res.y;
  }

  if( t>tmax ) m=-1.;
  return vec2( t, m );
}


float softshadow(in vec3 ro, in vec3 rd, in float mint, in float tmax) {
  float res = 1.;
  float t = mint;

  for( int i=0; i<16; i++ ) {
    float h = map( ro + rd*t, ro ).x;
    res = min( res, 8.*h/t );
    t += clamp( h, .02, .10 );
    if( h<.001 || t>tmax ) break;
  }

  return clamp( res, .0, 1. );
}

vec3 calcNormal(in vec3 pos) {
  vec2 e = vec2(1.,-1.)*.5773*.0005;
  return normalize( e.xyy*map( pos + e.xyy, pos ).x +
    e.yyx*map( pos + e.yyx, pos ).x +
    e.yxy*map( pos + e.yxy, pos ).x +
    e.xxx*map( pos + e.xxx, pos ).x );
}

float calcAO(in vec3 pos, in vec3 nor) {
  float occ = .0;
  float sca = 1.;

  for(int i=0; i<5; i++) {
    float hr = .01 + .12*float(i)/4.;
    vec3 aopos =  nor * hr + pos;
    float dd = map( aopos, pos ).x;
    occ += -(dd-hr)*sca;
    sca *= .95;
  }

  return clamp( 1. - 3.*occ, .0, 1. );
}

vec3 render(in vec3 ro, in vec3 rd) {
  vec3 col = vec3(.0);
  //vec3 col = vec3(.05, .05, .05) +rd.y*.1;
  vec2 res = castRay(ro,rd);
  float t = res.x;
  float m = res.y;
  if( m>-.5 ) {
    vec3 pos = ro + t*rd;
    vec3 nor = calcNormal( pos );
    vec3 ref = reflect( rd, nor );

    // material
    col = .45 + .35*sin( vec3(.05,.08,.10)*(m-1.0) );
    /*
    if( m<1.5 ) {
      float f = mod( floor(5.0*pos.z) + floor(5.0*pos.x), 2.0);
      col = .3 + .1*f*vec3(1.0);
    }
    */

    // lighitng. Seems Lighit
    float occ = calcAO( pos, nor );
    vec3  lig = normalize( vec3(-.4, .7, -.6) );
    float amb = clamp( .5+.5*nor.y, .0, 1. );
    float dif = clamp( dot( nor, lig ), .0, 1. );
    float bac = clamp( dot( nor, normalize(vec3(-lig.x,.0,-lig.z))), .0, 1. )*clamp( 1.-pos.y,.0,1.);
    float dom = smoothstep( -.1, .1, ref.y );
    float fre = pow( clamp(1.+dot(nor,rd),.0,1.), 2. );
    float spe = pow(clamp( dot( ref, lig ), .0, 1. ),16.);

    dif *= softshadow( pos, lig, .02, 2.5 );
    dom *= softshadow( pos, ref, .02, 2.5 );

    vec3 lin = vec3(.0);
    lin += 1.3*dif*vec3(1.,.8,.55);
    lin += 2.*spe*vec3(1.,.9,.7)*dif;
    lin += .4*amb*vec3(.4,.6,1.)*occ;
    lin += .5*dom*vec3(.4,.6,1.)*occ;
    lin += .5*bac*vec3(.25,.25,.25)*occ;
    lin += .25*fre*vec3(1.)*occ;
    col = col*lin;

    col = mix( col, vec3(.8,.9,1.), 1.-exp( -.0002*t*t*t ) );
  }

  return vec3( clamp(col,.0,1.) );
}

mat3 setCamera(in vec3 ro, in vec3 ta, float cr) {
	vec3 cw = normalize(ta-ro);
	vec3 cp = vec3(sin(cr), cos(cr),.0);
	vec3 cu = normalize( cross(cw,cp) );
	vec3 cv = normalize( cross(cu,cw) );

  return mat3( cu, cv, cw );
}

void main() {
  vec3 tot = vec3(.0);
  for( int m=0; m<2; m++ )   // 2x AA
  for( int n=0; n<2; n++ ) { // 2x AA
    // pixel coordinates
    vec2 o = vec2(float(m),float(n)) / float(2) - .5;
    vec2 p = (-b.xy + 3.*(gl_FragCoord.xy+o))/b.y;

    // camera
    // ro = ray origin = where the camera is
    // ta = camera direction (where the camera is looking)
    // cr = camera rotation
    //vec3 ro = vec3( -.5+3.5*cos(.1*a), 1.0, .5 + 4.0*sin(.1*a) );
    vec3 ro = vec3( -.5+.2*cos(.1*a), 1., .0 + 2.*sin(.1*a) );
    vec3 ta = vec3( .0 );
    // camera-to-world transformation
    mat3 ca = setCamera( ro, ta, .0 );
    // ray direction
    vec3 rd = ca * normalize( vec3(p.xy,2.) );

    // render
    vec3 col = render( ro, rd );

  	// gamma
    col = pow( col, vec3(.5) );

    tot += col;
  }

  tot /= 4.; // AA * AA

  gl_FragColor = vec4( tot, 1. );
}

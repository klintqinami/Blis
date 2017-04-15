@fragment void fshader(out vec3 color)
{
  vec3 x = vec3(0., 1., 2.);
  if ( x.x == 0. &&
       x.y == 1. &&
       x.z == 2.) {
    color = vec3(0., 1., 0.);
  } 
  else {
    color = vec3(1., 0., 0.);
  }
}

@fragment void fshader(out vec3 color)
{
  bvec3 tom = bvec3(true, false, true);
  ivec3 jeff = ivec3(-1, 1, 2);
  tom = !tom;
  jeff = -jeff;
  if(
    tom.x == false &&
    tom.y == true &&
    tom.z == false && 
    jeff.x == 1 &&
    jeff.y == -1 &&
    jeff.z == -2)
    color = vec3(0., 1., 0.);
  else
    color = vec3(1., 0., 0.);


}

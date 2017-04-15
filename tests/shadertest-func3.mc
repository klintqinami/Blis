@gpu vec3 fun(float x, float y, float z)
{
   return vec3(x,y,z);
}

@fragment void fshader(out vec3 color)
{
  color = fun(0., 1., 0.);
}

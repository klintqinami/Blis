@gpu void foo(){}

@gpu vec3 fun(float x, float y, float z)
{
   return vec3(x,y+1.,z);
}

@fragment void fshader(out vec3 color)
{
  color = fun(0., 0., 0.);
}

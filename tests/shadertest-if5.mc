@fragment void fshader(out vec3 color)
{
  int a = cond(true);
  int b = cond(false);

  if (a == 42 && b == 17)
    color = vec3(0., 1., 0.);
  else
    color = vec3(1., 0., 0.);
  
}

@gpu int cond(bool b)
{
  int x;
  if (b) x = 42; else x = 17; 
  return x;
}

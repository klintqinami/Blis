@gpu int one(inout int y)
{
  y = y + 1;
  return 0;
}

@fragment void fshader(out vec3 color)
{
  int y = 41;
  one(y);
  if ( y == 42)
    color = vec3(0., 1., 0.);
  else
    color = vec3(1., 0., 0.);
}

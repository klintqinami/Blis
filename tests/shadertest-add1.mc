@gpu int add(int x, int y)
{
  return x + y;
}

@fragment void fshader(out vec3 color)
{
  if (add(17, 25) == 42)
    color = vec3(0., 1., 0.);
  else
    color = vec3(1., 0., 0.);
}

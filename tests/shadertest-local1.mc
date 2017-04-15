@gpu int foo(bool i)
{
  int i; /* Should hide the formal i */

  i = 42;
  return i + i;
}

@fragment void fshader(out vec3 color)
{
  int result = foo(true);
  if (result == 84)
    color = vec3(0., 1., 0.);
  else
    color = vec3(1., 0., 0.);
}

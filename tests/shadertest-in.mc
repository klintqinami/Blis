@fragment void fshader(out vec3 color)
{
  foo(3);
  color = vec3(0., 1., 0.);
}

@gpu int foo(in int y)
{
  y = 42; 
  return y;
}

/*
 * This test currently fails because "in" is specified before the
 * type of the formal parameter.
 */

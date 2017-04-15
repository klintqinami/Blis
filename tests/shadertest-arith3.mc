@gpu int foo(int a)
{
  return a;
}

@fragment void fshader(out vec3 color)
{
  int a;
  a = 42;
  a = a + 5;
  if (a == 47) 
    color = vec3(0., 1., 0.);
  else
    color = vec3(1., 0., 0.);
}

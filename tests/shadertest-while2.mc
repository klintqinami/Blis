@gpu int foo(int a)
{
  int j;
  j = 0;
  while (a > 0) {
    j = j + 2;
    a = a - 1;
  }
  return j;
}

@fragment void fshader(out vec3 color)
{
  int result = foo(7);
  if (result == 14)
    color = vec3(0., 1., 0.);
  else 
    color = vec3(1., 0., 0.);
}

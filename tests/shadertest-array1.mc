@fragment void fshader(out vec3 color)
{
  bool error = false;
  int[5] a = int[5](0,1,2,42,4);
  int i = 0;

  a[3] = 3;
  while ( i < 5) {
    if (a[i] != i)
      error = true;
    i = i + 1;
  }
  if (error)
    color = vec3(1., 0., 0.);
  else
    color = vec3(0., 1., 0.);
}

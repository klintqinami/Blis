@fragment void fshader(out vec3 color)
{
  int i;
  int j = 7;
  for (i = 5; i > 0; i = i - 1) {
    j = 6;
  }
  if (i == 0)
    color = vec3(0., 1., 0.);
  else
    color = vec3(1., 0., 0.);
}

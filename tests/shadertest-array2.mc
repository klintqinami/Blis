@fragment void fshader(out vec3 color)
{
  int[2][3] a = int[2][3](int[3](1, 2, 3), int[3](4, 5, 6));
  color = vec3(0., 1., 0.);
}

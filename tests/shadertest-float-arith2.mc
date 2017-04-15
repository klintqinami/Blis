@fragment void fshader(out vec3 color)
{
  if (1. + 2. * 3. + 4. == 11.0)
    color = vec3(0., 1., 0.);
  else
    color = vec3(1., 0., 0.);
}

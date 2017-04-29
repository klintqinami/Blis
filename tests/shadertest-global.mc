const float FOO = 2.;

@fragment void fshader(out vec3 color)
{
  if (FOO == 2.)
    color = vec3(0., 1., 0.);
  else
    color = vec3(1., 0., 0.);
}

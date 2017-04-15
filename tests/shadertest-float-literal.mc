@fragment void fshader(out vec3 color)
{
  if (1.e-2 == .01 && .1 == 1.e-1 && 1. == 0.1e1 && 1000.0 == 1e+3)
    color = vec3(0., 1., 0.);
  else
    color = vec3(1., 0., 0.);
}

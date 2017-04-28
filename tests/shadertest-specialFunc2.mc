@fragment void fshader(out vec3 color)
{
  float x = pow(2., 3.);
    if (x - 8. < 1e-5)
    color = vec3(0., 1., 0.);
  else
    color = vec3(1., 0., 0.);
}

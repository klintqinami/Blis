@fragment void fshader(out vec3 color)
{
  float x = floor(1.5);
    if (x - 1.0  < 1e-5)
    color = vec3(0., 1., 0.);
  else
    color = vec3(1., 0., 0.);
}

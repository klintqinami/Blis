@fragment void fshader(out vec3 color)
{
  float x = sin(3.14159265);
  float y = sin(3.1415/2.);
  float z = cos(3.14159265);
  float w = cos(3.1415/2.);
 
    if (x < 1e-5 &&
        y-1. < 1e-5 &&
        z + 1. < 1e-5 &&
        w - 0.000046 < 1e-5)
    color = vec3(0., 1., 0.);
  else
    color = vec3(1., 0., 0.);
}

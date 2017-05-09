@fragment void fshader(out vec3 color)
{
  int a = 42;
  float b = float(a);
  int c = int(b);

  if (b == 42. &&
      c == 42 &&
      int(false) == 0 &&
      int(true) == 1 &&
      float(false) == 0. &&
      float(true) == 1. &&
      !bool(0) &&
      bool(1) &&
      !bool(0.) &&
      bool(1)) {
    color = vec3(0., 1., 0.);
  } else {
    color = vec3(1., 0., 0.);
  }
}

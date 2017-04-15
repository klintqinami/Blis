@gpu int gcd(int a, int b) {
  while (a != b) {
    if (a > b) a = a - b;
    else b = b - a;
  }
  return a;
}

@fragment void fshader(out vec3 color)
{
  if (gcd(14, 21) == 7 &&
      gcd(8, 36) == 4 &&
      gcd(99, 121) == 11)
    color = vec3(0., 1., 0.);
  else
    color = vec3(1., 0., 0.);
}

@fragment void fshader(out vec3 color)
{
  vec2 tom = vec2(1337., 42.);
  vec2 jeff = vec2(1., 2.);
  vec2 john = vec2(4., 5.);
  mat3x2 bob = mat3x2(tom, jeff, john);

  if (bob.x.x == 1337. &&
      bob.x.y == 42. &&
      bob.y.x == 1. &&
      bob.y.y == 2. &&
      bob.z.x == 4. &&
      bob.z.y == 5.)
    color = vec3(0., 1., 0.);
  else
    color = vec3(1., 0., 0.);
}

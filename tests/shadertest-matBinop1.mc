@fragment void fshader(out vec3 color)
{
  vec2 tom = vec2(1337., 42.);
  vec2 jeff = tom + tom;
  vec2 john = jeff / jeff;
  ivec2 x1 = ivec2(42, 1337);
  ivec2 x2 = ivec2(3, 3);
  ivec2 x3 = x1 + x2 + (x2 / x2);

  if (tom.x == 1337. &&
      tom.y == 42. &&
      jeff.x == 2674. &&
      jeff.y == 84. &&
      john.x == 1. &&
      john.y == 1. &&
      x3.x == 46 &&
      x3.y == 1341)
    color = vec3(0., 1., 0.);
  else
    color = vec3(1., 0., 0.);
}

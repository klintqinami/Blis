@fragment void fshader(out vec3 color)
{
    int x = 10;
    int y = 42;
    int z = y % x;

    ivec2 a = ivec2(x, y);
    ivec2 b = ivec2(x, x);
    ivec2 c = a % b;

   if (z == 2 &&
      c.x == 0 &&
      c.y == 2 )
    color = vec3(0., 1., 0.);
  else
    color = vec3(1., 0., 0.);
}

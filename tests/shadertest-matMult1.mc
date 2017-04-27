@fragment void fshader(out vec3 color)
{
    vec3 x1 = vec3(1., 2., 3.);
    mat3x1 x2 = mat3x1(4., 5., 6.);
    float x3 = x2 * x1;
    vec3 e1 = vec3(1., 0., 0.);
    vec3 e2 = vec3(0., 1., 0.);
    vec3 e3 = vec3(0., 0., 1.);
    mat3x3 id = mat3x3(e1, e2, e3);
    vec3 tom = vec3(1337., 42., 300.);
    tom = id * tom;

   if (x3 == 32. &&
      tom.x == 1337. &&
      tom.y == 42. &&
      tom.z == 300.)
    color = vec3(0., 1., 0.);
  else
    color = vec3(1., 0., 0.);
}

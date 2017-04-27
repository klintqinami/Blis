@fragment void fshader(out vec3 color)
{
  vec3 x1 = vec3(7., 0., -3.);
  vec3 x2 = vec3(2., 3., 4.);
  vec3 x3 = vec3(1., -1., -2.);
  mat3x3 A = mat3x3(x1, x2, x3);      
  vec3 x4 = vec3(-2., 3., 9.);
  vec3 x5 = vec3(8., -11., -34.);
  vec3 x6 = vec3(-5., 7., 21.);
  mat3x3 Ainv = mat3x3(x4, x5, x6);      
  mat3x3 I = A * Ainv;
  mat3x3 I2 = Ainv * A;
    if (
        I.x.x == 1. &&
        I.x.y == 0. &&
        I.x.z == 0. &&
        I.y.x == 0. &&
        I.y.y == 1. &&
        I.y.z == 0. &&
        I.z.x == 0. &&
        I.z.y == 0. &&
        I.z.z == 1. &&
        I2.x.x == 1. &&
        I2.x.y == 0. &&
        I2.x.z == 0. &&
        I2.y.x == 0. &&
        I2.y.y == 1. &&
        I2.y.z == 0. &&
        I2.z.x == 0. &&
        I2.z.y == 0. &&
        I2.z.z == 1. )
    color = vec3(0., 1., 0.);
  else
    color = vec3(1., 0., 0.);
}

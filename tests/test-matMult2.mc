int main() {
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
  printf(I.x.x);
  printf(I.x.y);
  printf(I.x.z);
  printf(I.y.x);
  printf(I.y.y);
  printf(I.y.z);
  printf(I.z.x);
  printf(I.z.y);
  printf(I.z.z);
  printf(I2.x.x);
  printf(I2.x.y);
  printf(I2.x.z);
  printf(I2.y.x);
  printf(I2.y.y);
  printf(I2.y.z);
  printf(I2.z.x);
  printf(I2.z.y);
  printf(I2.z.z);
  return 0;
}
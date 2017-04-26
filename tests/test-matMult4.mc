int main() {
  vec3 x1 = vec3(1., 2., 3.);
  mat3x1 A = mat3x1(4., 5., 6.);      
  mat3x3 P = x1 * A;
  printf(P.x.x);
  printf(P.x.y);
  printf(P.x.z);
  printf(P.y.x);
  printf(P.y.y);
  printf(P.y.z);
  printf(P.z.x);
  printf(P.z.y);
  printf(P.z.z);
  return 0;
}

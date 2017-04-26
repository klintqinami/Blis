int main() {
  vec3 x1 = vec3(1., 2., 3.);
  mat3x1 A = mat3x1(4., 5., 6.);      
  mat3x3 P = x1 * A;
  A = A * P;
  printf(A.x);
  printf(A.y);
  printf(A.z);
  return 0;
}

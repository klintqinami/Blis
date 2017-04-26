int main() {
  float scalar = 1337.;
  mat1x4 colvec = mat1x4(1., 2., 3., 4.);
  mat1x4 prod2 = colvec * scalar;
  mat4x1 rowvec = mat4x1(1., 2., 3., 4.);
  mat4x1 prod = scalar * rowvec;
  printf(prod.x);
  printf(prod.y);
  printf(prod.z);
  printf(prod.w);
  printf(prod2.x);
  printf(prod2.y);
  printf(prod2.z);
  printf(prod2.w);
  return 0;
}

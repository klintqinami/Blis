@fragment void fshader(out vec3 color)
{
  float scalar = 1337.;
  mat1x4 colvec = mat1x4(1., 2., 3., 4.);
  mat1x4 prod2 = colvec * scalar;
  mat4x1 rowvec = mat4x1(1., 2., 3., 4.);
  mat4x1 prod = scalar * rowvec;
    if (
        prod.x == 1337. &&
        prod.y == 2674. &&
        prod.z == 4011. &&
        prod.w == 5348. &&
        prod2.x == 1337. &&
        prod2.y == 2674. &&
        prod2.z == 4011. &&
        prod2.w == 5348.)
    color = vec3(0., 1., 0.);
  else
    color = vec3(1., 0., 0.);
}

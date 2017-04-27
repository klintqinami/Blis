@fragment void fshader(out vec3 color)
{
  vec3 x1 = vec3(1., 2., 3.);
  mat3x1 A = mat3x1(4., 5., 6.);      
  mat3x3 P = x1 * A;
  A = A * P;
    if (
        A.x == 128. &&
        A.y == 160. &&
        A.z == 192. )
    color = vec3(0., 1., 0.);
  else
    color = vec3(1., 0., 0.);
}

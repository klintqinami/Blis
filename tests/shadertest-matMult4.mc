@fragment void fshader(out vec3 color)
{
  vec3 x1 = vec3(1., 2., 3.);
  mat3x1 A = mat3x1(4., 5., 6.);      
  mat3x3 P = x1 * A;
    if (
        P.x.x == 4. &&
        P.x.y == 8. &&
        P.x.z == 12. &&
        P.y.x == 5. &&
        P.y.y == 10. &&
        P.y.z == 15. &&
        P.z.x == 6. &&
        P.z.y == 12. &&
        P.z.z == 18.)
    color = vec3(0., 1., 0.);
  else
    color = vec3(1., 0., 0.);
}

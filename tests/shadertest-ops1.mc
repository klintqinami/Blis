@fragment void fshader(out vec3 color)
{
  if(
    1 + 2 == 3 &&
    1 - 2 == -1 &&
    1 * 2 == 2 &&
    100 / 2 == 50 &&
    (1 == 2) == false &&
    (1 == 1) == true &&
    (1 != 2) == true &&
    (1 != 1) == false &&
    (1 < 2) == true &&
    (2 < 1) == false &&
    (1 <= 2) == true &&
    (1 <= 1) == true &&
    (2 <= 1) == false &&
    (1 > 2) == false &&
    (2 > 1) == true &&
    (1 >= 2) == false &&
    (1 >= 1) == true &&
    (2 >= 1) == true)
    color = vec3(0., 1., 0.);
  else
    color = vec3(1., 0., 0.);


}

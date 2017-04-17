@fragment void fshader(out vec3 color)
{
  int i = 0;
  while((i = 2*(i + 1)) == 2){
    i = i + 5;
  }
  if (i == 16)
    color = vec3(0., 1., 0.);
  else
    color = vec3(1., 0., 0.);
}

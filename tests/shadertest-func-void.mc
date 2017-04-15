@fragment void fshader(out vec3 color)
{
  one();
  color = vec3(0., 1., 0.);
}

@gpu void one()
{
  int y = 42; 
}

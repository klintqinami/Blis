@fragment void fshader(out vec3 color)
{
  bool error = false;
  int i = 42;

  {
    // still 42
    if (i != 42)
      error = true;

    int i = 43;
    // now 43
    if (i != 43)
      error = true;
  }

  // 42 again
  if (i != 42)
    error = true;
  
  if(error)
    color = vec3(1., 0., 0.);
  else
    color = vec3(0., 1., 0.);
}

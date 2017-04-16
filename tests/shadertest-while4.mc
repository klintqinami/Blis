@fragment void fshader(out vec3 color)
{
  int i = 0;
  while ( i < 10) {
    if ( i == 5 ) {
      i = 32;
      continue;
    }
    i = i + 1;
  }
  if ( i == 32 ) 
    color = vec3(0., 1., 0.);
  else
    color = vec3(1., 0., 0.);
}

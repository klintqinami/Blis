@gpu vec3 foo()
{
  return vec3(0., 1., 0.);
}

@fragment void fshader(out vec3 color)
{
  color = foo();
}

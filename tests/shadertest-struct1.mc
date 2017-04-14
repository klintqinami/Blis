struct foo {
  int a;
};

@fragment void fshader(out vec3 color)
{
  struct foo bar;
  bar.a = 42;
  if (bar.a == 42)
    color = vec3(0., 1., 0.);
  else
    color = vec3(1., 0., 0.);
}

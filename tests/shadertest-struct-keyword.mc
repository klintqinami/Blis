struct foo {
  // "attribute" is a keyword in GLSL
  int attribute;
};

@fragment void fshader(out vec3 color)
{
  struct foo bar;
  bar.attribute = 42;
  if (bar.attribute == 42)
    color = vec3(0., 1., 0.);
  else
    color = vec3(1., 0., 0.);
}

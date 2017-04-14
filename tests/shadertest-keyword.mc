// "attribute" is a keyword in GLSL but not Blis
@gpu vec3 attribute()
{
  return vec3(0., 1., 0.);
}

@fragment void fshader(out vec3 color)
{
  color = attribute();
}

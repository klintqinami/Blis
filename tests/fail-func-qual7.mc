@vertex vec4 vshader(vec3 pos)
{
  return vec4(pos.x, pos.y, pos.z, 1.);
}

@fragment void fshader(out vec3 color)
{
  color = vec3(1., 0., 1.);
}

pipeline my_pipeline {
  @vertex vshader;
  @fragment fshader;
};

pipeline my_pipeline foo;

@gpuonly void bar()
{
  bind_pipeline(foo);
}

int main()
{
  return 0;
}


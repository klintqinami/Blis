pipeline my_pipeline {
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


@gpuonly void foo() {}

@gpu void bar()
{
  foo();
}

int main()
{
  bar();
  return 0;
}

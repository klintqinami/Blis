@vertex void foo() {}

@gpuonly void bar()
{
  foo();
}

int main()
{
  return 0;
}

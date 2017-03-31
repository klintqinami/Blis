@vertex void foo() {}

@fragment void bar()
{
  foo();
}

int main()
{
  return 0;
}

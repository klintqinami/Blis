int foo()
{
  return 42;
}

int main()
{
  foo() = 1; // Error: "foo()" is not an lvalue
}

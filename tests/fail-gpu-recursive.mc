@gpu void foo() {
  bar();
}

@gpu void bar() {
  foo();
}

int main()
{
  return 0;
}

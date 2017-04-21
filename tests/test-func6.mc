void foo() {}

int bar(int a, bool b, int c) { return a + c; }

int main()
{
  printi(bar(17, false, 25));
  return 0;
}

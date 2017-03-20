int foo(int a, bool b, int c) { }

void bar(int a, baz b, int c) {} /* Error: struct baz does not exist */

int main()
{
  return 0;
}

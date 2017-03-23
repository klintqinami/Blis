int foo(int a, bool b, int c) { }

struct baz bar(int a, bool b, int c) {} /* Error: struct baz does not exist */

int main()
{
  return 0;
}

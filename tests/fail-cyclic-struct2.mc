struct foo {
  struct bar a;
};

struct bar {
  struct baz b;
};

struct baz {
  struct foo c;
};

int main()
{
}

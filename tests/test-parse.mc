int main()
{
  int a;
  printb(string_to_integer("42", a));
  printi(a);

  float b;
  printb(string_to_float("42.", b));
  printf(b);

  printb(string_to_float("-0.1", b));
  printf(b);

  return 0;
}


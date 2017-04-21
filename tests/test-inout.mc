void one(inout int y)
{
  printi(y);
  y = 42;
}

int main()
{
  int y = 1;
  one(y);
  printi(y);
  return 0;
}

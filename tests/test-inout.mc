void one(inout int y)
{
  print(y);
  y = 42;
}

int main()
{
  int y = 1;
  one(y);
  print(y);
  return 0;
}

int one(in int y)
{
  y = 42;
  print(y);
  return y;
}

int main()
{
  int y = 1;
  one(y);
  print(y);
  return 0;
}

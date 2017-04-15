int one(out int y)
{
  y = 42;
  return 0;
}

int main()
{
  int y;
  one(y);
  print(y);
  return 0;
}

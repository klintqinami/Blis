void one(out int y)
{
  y = 42;
}

int main()
{
  int y;
  one(y);
  print(y);
  return 0;
}

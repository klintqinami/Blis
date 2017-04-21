int one(out int y)
{
  y = 42;
  return 0;
}

int main()
{
  int y;
  one(y);
  printi(y);
  return 0;
}

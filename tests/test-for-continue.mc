int main()
{
  int i;
  for (i = 0 ; i < 5 ; i = i + 1) {
    if (i == 3)
      continue;
    print(i);
  }
  print(42);
  return 0;
}

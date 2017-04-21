int main()
{
  int[5] a = int[5](0, 1, 2, 3, 4);
  int i;

  a[3] = 42;
  for (i = 0; i < 5; i = i + 1)
    printi(a[i]);

  return 0;
}

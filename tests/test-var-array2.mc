void arr_modify(int[] a)
{
  a[1] = 42;
}


int main()
{
  int[] a = int[](2);
  a[0] = 1;
  a[1] = 2;
  arr_modify(a);
  printi(a[1]);
  return 0;
}

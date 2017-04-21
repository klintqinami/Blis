int main()
{
  int i = 42;

  {
    printi(i); // still 42

    int i = 43;
    printi(i); // now 43
  }

  // 42 again
  printi(i);

  return 0;
}

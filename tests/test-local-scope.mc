int main()
{
  int i = 42;

  {
    print(i); // still 42

    int i = 43;
    print(i); // now 43
  }

  // 42 again
  print(i);

  return 0;
}

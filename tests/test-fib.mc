int fib(int x)
{
  if (x < 2) return 1;
  return fib(x-1) + fib(x-2);
}

int main()
{
  printi(fib(0));
  printi(fib(1));
  printi(fib(2));
  printi(fib(3));
  printi(fib(4));
  printi(fib(5));
  return 0;
}

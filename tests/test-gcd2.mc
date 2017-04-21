int gcd(int a, int b) {
  while (a != b)
    if (a > b) a = a - b;
    else b = b - a;
  return a;
}

int main()
{
  printi(gcd(14,21));
  printi(gcd(8,36));
  printi(gcd(99,121));
  return 0;
}

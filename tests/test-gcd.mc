int gcd(int a, int b) {
  while (a != b) {
    if (a > b) a = a - b;
    else b = b - a;
  }
  return a;
}

int main()
{
  printi(gcd(2,14));
  printi(gcd(3,15));
  printi(gcd(99,121));
  return 0;
}

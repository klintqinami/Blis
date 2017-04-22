int main()
{
  u8 a = '\n';
  printb(a == '\n'); // true
  printb('a' == a); // false
  printb(a != '\n'); // false
  printb('a' != a); // true
  return 0;
}

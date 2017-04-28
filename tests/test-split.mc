void print_string_list(u8[][] strings)
{
  int i;
  for (i = 0; i < length(strings); i = i + 1) {
    print(strings[i]);
    print("\n");
  }
}

int main()
{
  print_string_list(split("aababba", 'a'));
  print("\n");
  print_string_list(split("babb", 'a'));
  print("\n");
  print_string_list(split("abbbbbbbba", 'a'));

  return 0;
}

int main()
{
  u8[18] path = "./tests/test-io.mc";
  u8[] fpath = u8[](length(path));
  int i;
  for (i = 0; i < length(path); i = i + 1)
    fpath[i] = path[i];
  u8[] file = read_file(fpath);
  print(file);
  return 0;
}

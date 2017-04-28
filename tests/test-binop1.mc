int main() {
  bvec2 x = bvec2(true, false);
  bvec2 y = bvec2(false, true);

  bvec2 z = x && y;
  bvec2 w = x || y;
  printb(z.x);
  printb(z.y);
  printb(w.x);
  printb(w.y);
  return 0;
}

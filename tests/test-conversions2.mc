int main()
{
  bvec2 a = bvec2(false, true);
  ivec2 b = ivec2(a);
  vec2 c = vec2(a);
  vec2 d = vec2(b);
  ivec2 e = ivec2(d);

  printb(a.x);
  printb(a.y);
  printi(b.x);
  printi(b.y);
  printf(c.x);
  printf(c.y);
  printf(d.x);
  printf(d.y);
  printi(e.x);
  printi(e.y);
  return 0;
}

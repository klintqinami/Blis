int main() {
    int x = 10;
    int y = 42;
    int z = y % x;

    ivec2 a = ivec2(x, y);
    ivec2 b = ivec2(x, x);
    ivec2 c = a % b;
    printi(z);
    printi(c.x);
    printi(c.y);
    return 0;
}

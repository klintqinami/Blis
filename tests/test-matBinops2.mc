int main() {
    ivec2 x1 = ivec2(1337, 42);
    ivec2 x2 = ivec2(2, 2);
    ivec2 x3 = x2 * x1 - x1;
    ivec2 x4 = x1 / x2;
    printi(x1.x);
    printi(x1.y);
    printi(x3.x);
    printi(x3.y);
    printi(x4.x);
    printi(x4.y);
    return 0;
}

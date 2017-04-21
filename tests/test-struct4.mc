struct foo {
    int a;
    float b;
};

int main() {
    struct foo temp = foo(42, 1337.0);
    printi(temp.a);
    printf(temp.b);

    return 0;   
}

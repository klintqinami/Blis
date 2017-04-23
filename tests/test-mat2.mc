int main() {
    vec2 tom = vec2(1337., 42.);
    vec2 jeff = vec2(1., 2.);
    vec2 john = vec2(4., 5.);
    mat3x2 bob = mat3x2(tom, jeff, john);
    printf(bob.x.x);
    printf(bob.x.y);
    printf(bob.y.x);
    printf(bob.y.y);
    printf(bob.z.x);
    printf(bob.z.y);
    return 0;
}

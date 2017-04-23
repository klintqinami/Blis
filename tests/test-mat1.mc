int main() {
    vec3 tom = vec3(1337., 42., 300.);
    vec3 jeff = vec3(1., 2., 3.);
    vec3 john = vec3(4., 5., 6.);
    mat3x3 bob = mat3x3(tom, jeff, john);
    printf(bob.x.x);
    printf(bob.x.y);
    printf(bob.x.z);
    printf(bob.y.x);
    printf(bob.y.y);
    printf(bob.y.z);
    printf(bob.z.x);
    printf(bob.z.y);
    printf(bob.z.z);
    return 0;
}

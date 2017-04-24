int main() {
    vec3 x1 = vec3(1337., 42., 300.);
    vec3 x2 = x1 + x1;
    vec3 x3 = vec3(2., 2., 2.);
    vec3 x4 = x3 * x1;
    vec3 x5 = x1 / x2;
    mat4x3 bob = mat4x3(x1, x2, x4, x5);
    bob = bob + bob;
    printf(bob.x.x);
    printf(bob.x.y);
    printf(bob.x.z);
    printf(bob.y.x);
    printf(bob.y.y);
    printf(bob.y.z);
    printf(bob.z.x);
    printf(bob.z.y);
    printf(bob.z.z);
    printf(bob.w.x);
    printf(bob.w.y);
    printf(bob.w.z);
    return 0;
}

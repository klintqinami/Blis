int main() {
    vec3 x1 = vec3(1., 2., 3.);
    bool x1x1 = x1 == x1;
    mat3x1 x2 = mat3x1(4., 5., 6.);
    bool x2x2 = x2 == x2;
    vec3 e1 = vec3(1., 0., 0.);
    vec3 e2 = vec3(0., 1., 0.);
    bool e1e2 = e1 == e2;
    vec3 e3 = vec3(0., 0., 1.);
    mat3x3 id = mat3x3(e1, e2, e3);
    bool idid = id == id;
    vec3 tom = vec3(1337., 42., 300.);
    mat3x3 id2 = 2. * id;
    bool id2gid = id2 >= id;
    bool id2lid = id2 <= id;
    printb(x1x1);
    printb(x2x2);
    printb(e1e2);
    printb(idid);
    printb(id2gid);
    printb(id2lid);
    return 0;
}

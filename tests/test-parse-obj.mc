int main()
{
  vec3[] verts;
  int[] faces;
  if (!read_obj("./kitten.obj", verts, faces)) {
    print("error reading obj\n");
    return 1;
  }

  printi(length(verts));
  printi(length(faces) / 3);

  return 0;
}

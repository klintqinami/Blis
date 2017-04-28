@vertex vec4 vshader(vec3 pos)
{
  return vec4(pos.x, pos.y, pos.z, 1.);
}

// the test fills out the fragment shader
/*
@fragment void fshader(out vec3 color)
{
  color = vec3(0., 1., 0.);
}
*/

pipeline my_pipeline {
  @vertex vshader;
  @fragment fshader;
};

int main()
{
  window win = window(10, 10, true);

  set_active_window(win);

  buffer<vec3> b = buffer<vec3>();

  upload_buffer(b,
    vec3[6](vec3(-1., -1., 0.),
            vec3(1.,  -1., 0.),
            vec3(1.,  1.,  0.),
            vec3(1., 1., 0.),
            vec3(-1., -1., 0.),
            vec3(-1., 1., 0.)));

  pipeline my_pipeline p = pipeline my_pipeline(false);
  p.pos = b;

  draw(p, 6);

  int x;
  int y;

  for (y = 0; y < 10; y = y + 1) {
    for (x = 0; x < 10; x = x + 1) {
      vec4 pixel = read_pixel(x, y);
      if (pixel.x != 0. || pixel.y != 1. || pixel.z != 0.) {
        printi(x);
        printi(y);
        printf(pixel.x);
        printf(pixel.y);
        printf(pixel.z);
        printf(pixel.w);
        return 1;
      }
    }
  }

  return 0;
}

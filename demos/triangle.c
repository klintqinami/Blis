#include <stdio.h>
#include <stdlib.h>
#ifdef __APPLE__
#include <OpenGL/gl3.h> /* Apple, y u special? */
#else
#include <GL/glew.h>
#endif
#include <GLFW/glfw3.h> /* window creation and input handling crap */
#include <stdbool.h> /* for true */

/* ------ Blis builtin functions ------ */

struct pipeline {
  GLuint vertex_array;
  GLuint program;
};

struct buffer {
  GLuint buffer;
};

GLuint compile_shader(const char *source, GLenum stage)
{
  GLuint shader = glCreateShader(stage);
  glShaderSource(shader, 1, &source, NULL);
  glCompileShader(shader);

  GLint result;
  glGetShaderiv(shader, GL_COMPILE_STATUS, &result);
  if (!result) {
    GLint log_length;
    glGetShaderiv(shader, GL_INFO_LOG_LENGTH, &log_length);
    char *error = malloc(log_length + 1);
    glGetShaderInfoLog(shader, log_length, NULL, error);
    printf("error compiling shader source:\n\n%s\n\n------\n\n%s\n", source, error);
    free(error);
    exit(1);
  }

  return shader;
}

struct pipeline create_pipeline(const char *vshader_source, const char *fshader_source)
{
  struct pipeline p;

  // compile shaders
  GLuint vshader = compile_shader(vshader_source, GL_VERTEX_SHADER);
  GLuint fshader = compile_shader(fshader_source, GL_FRAGMENT_SHADER);

  // link shaders
  p.program = glCreateProgram();
  glAttachShader(p.program, vshader);
  glAttachShader(p.program, fshader);
  glLinkProgram(p.program);

  GLint result;
  glGetProgramiv(p.program, GL_LINK_STATUS, &result);
  if (!result) {
    GLint log_length;
    glGetProgramiv(p.program, GL_INFO_LOG_LENGTH, &log_length);
    char *error = malloc(log_length + 1);
    glGetProgramInfoLog(p.program, log_length, NULL, error);
    printf("error linking vertex shader:\n\n%s\n\n-------\nand fragment shader:\n\n%s\n\n------\n\n%s\n",
            vshader_source, fshader_source, error);
    free(error);
    exit(1);
  }

  glGenVertexArrays(1, &p.vertex_array);

  return p;
}

struct buffer create_buffer()
{
  struct buffer b;
  glGenBuffers(1, &b.buffer);
  return b;
}

/* void *data and size are replaced by an array */
void buffer_upload(struct buffer *b, const void *data, size_t size, int hint)
{
  /* the target doesn't really matter */
  glBindBuffer(GL_ARRAY_BUFFER, b->buffer);
  glBufferData(GL_ARRAY_BUFFER, size, data, hint);
}

/* location corresponds to an input in the vertex shader */
void pipeline_bind_vertex_buffer(struct pipeline *p, struct buffer *b, int location)
{
  glBindVertexArray(p->vertex_array);
  glVertexAttribPointer(location,
                        3, GL_FLOAT, false, /* vec3, unnormalized - comes from type of buffer */
                        0, /* stride */
                        (void *) 0 /* array buffer offset */
                       );
  glEnableVertexAttribArray(location);
}

void bind_pipeline(struct pipeline *p)
{
  glUseProgram(p->program);
  glBindVertexArray(p->vertex_array);
}

/* ------ Shaders (each generated from a @vertex or @fragment decorated entrypoint) ------ */

const char *vshader = "#version 330 core\n\
layout(location = 0) in vec3 in_pos; // location corresponds to location in pipeline_bind_vertex_buffer() \n\
\n\
void main() {\n\
  gl_Position = vec4(in_pos, 1.0); /* special output variable */\n\
}";

const char *fshader = "#version 330 core\n\
out vec3 color;\n\
\n\
void main() {\n\
  color = vec3(1.0, 0.0, 0.0);\n\
}";

int main()
{
  if (!glfwInit()) {
    fprintf(stderr, "failed to initialize glfw\n");
    return 1;
  }

  /* OpenGL 3.3, core profile */
  glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
  glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);
  glfwWindowHint(GLFW_OPENGL_FORWARD_COMPAT, GL_TRUE); /* To make MacOS happy; should not be needed */
  glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE); /* We don't want the old OpenGL */

  GLFWwindow *window;
  window = glfwCreateWindow(1024, 768, "Blis", NULL, NULL);
  if (!window) {
    fprintf(stderr, "failed to create window\n");
    return 1;
  }

  glfwMakeContextCurrent(window);

#ifndef __APPLE__
  glewExperimental = true;
  if (glewInit() != GLEW_OK) {
    fprintf(stderr, "Failed to initialize GLEW\n");
    return 1;
  }
#endif 


  /* my_pipeline p = my_pipeline() */
  struct pipeline p = create_pipeline(vshader, fshader);

  /* buffer<vec3> b = buffer<vec3>() */
  struct buffer b = create_buffer();

  /* vec3[] data = [vec3(...), vec3(...), vec3(...)] */
  const float data[] = {
    -1.0f, -1.0f, 0.0f,
    1.0f, -1.0f, 0.0f,
    0.0f, 1.0f, 0.0f
  };

  /* upload(b, data, STATIC_DRAW) */
  buffer_upload(&b, data, sizeof(data), GL_STATIC_DRAW);

  /* p.in_pos = b */
  pipeline_bind_vertex_buffer(&p, &b, 0);

  do {
    bind_pipeline(&p); /* use our pipeline for drawin' some triangles */
    glDrawArrays(GL_TRIANGLES, 0, 3); /* Go! */
    glfwSwapBuffers(window);
    glfwPollEvents();
  } while (glfwGetKey(window, GLFW_KEY_ESCAPE) != GLFW_PRESS &&
           glfwWindowShouldClose(window) == 0);
}


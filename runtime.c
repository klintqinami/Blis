#include <stdio.h>
#include <stdlib.h>
#ifdef __APPLE__
#include <OpenGL/gl3.h> /* Apple, y u special? */
#else
#include <GL/glew.h>
#endif
#include <GLFW/glfw3.h> /* window creation and input handling crap */
#include <stdbool.h> /* for true */
#include <string.h>
#include <assert.h>

struct pipeline {
  GLuint vertex_array;
  GLuint index_buffer;
  GLuint program;
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

void create_pipeline(struct pipeline *p,
                     const char *vshader_source, const char *fshader_source)
{
  // compile shaders
  GLuint vshader = compile_shader(vshader_source, GL_VERTEX_SHADER);
  GLuint fshader = compile_shader(fshader_source, GL_FRAGMENT_SHADER);

  // link shaders
  p->program = glCreateProgram();
  glAttachShader(p->program, vshader);
  glAttachShader(p->program, fshader);
  glLinkProgram(p->program);

  GLint result;
  glGetProgramiv(p->program, GL_LINK_STATUS, &result);
  if (!result) {
    GLint log_length;
    glGetProgramiv(p->program, GL_INFO_LOG_LENGTH, &log_length);
    char *error = malloc(log_length + 1);
    glGetProgramInfoLog(p->program, log_length, NULL, error);
    printf("error linking vertex shader:\n\n%s\n\n-------\nand fragment shader:\n\n%s\n\n------\n\n%s\n",
            vshader_source, fshader_source, error);
    free(error);
    exit(1);
  }

  glGenVertexArrays(1, &p->vertex_array);
  p->index_buffer = 0;
}

GLuint create_buffer(void)
{
  GLuint b;
  glGenBuffers(1, &b);
  return b;
}

/* void *data and size are replaced by an array */
void upload_buffer(GLuint buffer, const void *data, unsigned size, int hint)
{
  /* the target doesn't really matter */
  glBindBuffer(GL_ARRAY_BUFFER, buffer);
  glBufferData(GL_ARRAY_BUFFER, size, data, hint);
}

/* location corresponds to an input in the vertex shader */
void pipeline_bind_vertex_buffer(struct pipeline *p, GLuint b, int components, int location)
{
  glBindVertexArray(p->vertex_array);
  glBindBuffer(GL_ARRAY_BUFFER, b);
  glVertexAttribPointer(location,
                        components, GL_FLOAT, false, /* vecN - comes from type of buffer */
                        0, /* stride */
                        (void *) 0 /* array buffer offset */
                       );
  glEnableVertexAttribArray(location);
}

GLuint pipeline_get_vertex_buffer(struct pipeline *p, int location)
{
  GLuint out;
  glBindVertexArray(p->vertex_array);
  glGetVertexAttribIuiv(location, GL_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING, &out);
  return out;
} 

int pipeline_get_uniform_location(struct pipeline *p, char *name)
{
  return glGetUniformLocation(p->program, name);
}

void pipeline_set_uniform_float(struct pipeline *p, int location,
                                float *values, int rows, int cols)
{
  glUseProgram(p->program);
  switch (cols) {
    case 1:
      switch (rows) {
        case 1:
          glUniform1fv(location, 1, values);
          break;
        case 2:
          glUniform2fv(location, 1, values);
          break;
        case 3:
          glUniform3fv(location, 1, values);
          break;
        case 4:
          glUniform4fv(location, 1, values);
          break;
        default:
          assert(!"unreachable");
      }
      break;
    case 2:
      switch (rows) {
        case 1:
          glUniform2fv(location, 1, values);
          break;
        case 2:
          glUniformMatrix2fv(location, 1, false, values);
          break;
        case 3:
          glUniformMatrix2x3fv(location, 1, false, values);
          break;
        case 4:
          glUniformMatrix2x4fv(location, 1, false, values);
          break;
        default:
          assert(!"unreachable");
      }
      break;
    case 3:
      switch (rows) {
        case 1:
          glUniform3fv(location, 1, values);
          break;
        case 2:
          glUniformMatrix3x2fv(location, 1, false, values);
          break;
        case 3:
          glUniformMatrix3fv(location, 1, false, values);
          break;
        case 4:
          glUniformMatrix3x4fv(location, 1, false, values);
          break;
        default:
          assert(!"unreachable");
      }
      break;
    case 4:
      switch (rows) {
        case 1:
          glUniform4fv(location, 1, values);
          break;
        case 2:
          glUniformMatrix4x2fv(location, 1, false, values);
          break;
        case 3:
          glUniformMatrix4x3fv(location, 1, false, values);
          break;
        case 4:
          glUniformMatrix4fv(location, 1, false, values);
          break;
        default:
          assert(!"unreachable");
      }
      break;
    default:
      assert(!"unreachable");
  }
}

void pipeline_set_uniform_int(struct pipeline *p, int location,
                              int *values, int rows, int cols)
{
  glUseProgram(p->program);
  switch (cols) {
    case 1:
      switch (rows) {
        case 1:
          glUniform1iv(location, 1, values);
          break;
        case 2:
          glUniform2iv(location, 1, values);
          break;
        case 3:
          glUniform3iv(location, 1, values);
          break;
        case 4:
          glUniform4iv(location, 1, values);
          break;
        default:
          assert(!"unreachable");
      }
      break;
    default:
      assert(!"unreachable");
  }
}

void pipeline_get_uniform_float(struct pipeline *p, int location,
                                float *values)
{
  glUseProgram(p->program);
  glGetUniformfv(p->program, location, values);
}

void pipeline_get_uniform_int(struct pipeline *p, int location,
                              int *values)
{
  glUseProgram(p->program);
  glGetUniformiv(p->program, location, values);
}

void read_pixel(int x, int y, float *pixel)
{
  glReadPixels(x, y, 1, 1, GL_RGBA, GL_FLOAT, pixel);
}

void init(void)
{
  if (!glfwInit()) {
    fprintf(stderr, "failed to initialize glfw\n");
    exit(1);
  }

  /* OpenGL 3.3, core profile */
  glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
  glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);
  glfwWindowHint(GLFW_OPENGL_FORWARD_COMPAT, GL_TRUE); /* To make MacOS happy; should not be needed */
  glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE); /* We don't want the old OpenGL */

}

/* TODO: when we get string support, add a string for the name */
GLFWwindow *create_window(int width, int height, int offscreen)
{
  if (offscreen)
    glfwWindowHint(GLFW_VISIBLE, false);
  else
    glfwWindowHint(GLFW_VISIBLE, true);

  GLFWwindow *window = glfwCreateWindow(width, height, "Blis", NULL, NULL);
  if (!window) {
    fprintf(stderr, "failed to create window\n");
    exit(1);
  }

  return window;
}

void set_active_window(GLFWwindow *window)
{
  glfwMakeContextCurrent(window);

#ifndef __APPLE__
  glewExperimental = true;
  if (glewInit() != GLEW_OK) {
    fprintf(stderr, "Failed to initialize GLEW\n");
    exit(1);
  }
#endif 
}

void draw_arrays(struct pipeline *p, int num_indices)
{
  glUseProgram(p->program);
  glBindVertexArray(p->vertex_array);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, p->index_buffer);
  if (p->index_buffer)
    glDrawElements(GL_TRIANGLES, num_indices, GL_UNSIGNED_INT, (void*)0);
  else
    glDrawArrays(GL_TRIANGLES, 0, num_indices); /* Go! */
}

void swap_buffers(GLFWwindow *window)
{
  glfwSwapBuffers(window);
}

void poll_events(void)
{
  glfwPollEvents();
}

bool should_close(GLFWwindow *window)
{
  return glfwWindowShouldClose(window);
}

struct blis_string {
  int size;
  char *str; // note: NOT NUL-terminated
};

void read_file(struct blis_string *file, struct blis_string path)
{
  char *fpath = malloc(path.size + 1);
  // Out-of-memory error? What error?
  memcpy(fpath, path.str, path.size);
  fpath[path.size] = '\0';

  FILE *f = fopen(fpath, "r");
  if (!f) {
    fprintf(stderr, "couldn't open file %s\n", fpath);
    exit(1);
  }

  fseek(f, 0, SEEK_END);
  int fsize = ftell(f); // 2^31 ought to be large enough for everyone...
  fseek(f, 0, SEEK_SET);

  file->size = fsize;
  file->str = malloc(fsize);
  fread(file->str, fsize, 1, f);

  fclose(f);
}

void print_string(struct blis_string str)
{
  fwrite(str.str, str.size, 1, stdout);
}


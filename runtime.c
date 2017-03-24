#include <stdio.h>
#include <stdlib.h>
#ifdef __APPLE__
#include <OpenGL/gl.h> /* Apple, y u special? */
#else
#include <GL/glew.h>
#endif
#include <GLFW/glfw3.h> /* window creation and input handling crap */
#include <stdbool.h> /* for true */

struct pipeline {
  GLuint vertex_array;
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
void pipeline_bind_vertex_buffer(struct pipeline *p, GLuint b, int location)
{
  glBindVertexArray(p->vertex_array);
  glBindBuffer(GL_ARRAY_BUFFER, b);
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
GLFWwindow *create_window(int width, int height)
{
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

void draw_arrays(int num_indices)
{
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
  glfwWindowShouldClose(window);
}


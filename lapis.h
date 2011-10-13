#ifndef __LAPIS_H__
#define __LAPIS_H__

#include <memory.h>
#include <assert.h>
#include <time.h>
#include <limits.h>

#include <SDL.h>
#include <GL/gl.h>
#include <GL/glu.h>
#include <SDL_image.h>
#include <SDL_ttf.h>
#include <SDL_mixer.h>

#ifdef NDEBUG
# define LOG
#else
# define LOG(...) { fprintf(stdout, "[L] %s:%d -> ", __FILE__, __LINE__); fprintf(stdout, __VA_ARGS__); }
#endif

#define WARN(...) { fprintf(stdout, "[W] %s:%d -> ", __FILE__, __LINE__); fprintf(stdout, __VA_ARGS__); }
#define ERROR(...) { fprintf(stdout, "[E] %s:%d -> ", __FILE__, __LINE__); fprintf(stdout, __VA_ARGS__); }

#ifndef FALSE
# define FALSE 0
# define TRUE (!FALSE)
#endif

/* sdl_graphics_context */

unsigned int lsdl_get_tick();
void lsdl_set_video_mode(unsigned int screen_width,
                         unsigned int screen_height,
                         unsigned char fullscreen,
                         unsigned char resizable);
void lsdl_resize_internal(int w, int h);
void lsdl_fill_rect(float x, float y, 
                    float w, float h, 
                    float red, float green, float blue);
void lsdl_draw_image(GLuint texture,
                     float x, float y, float w, float h,
                     float r, float g, float b);
void lsdl_draw_text(char *font_name, int pt_size,
                    char *text, int r, int g, int b,
                    int x, int y);
void lsdl_flip();
void lsdl_prepare_render();
void lsdl_enable_smooth_lines();
void lsdl_draw_line(float sx, float sy, float ex, float ey, float sr, float sg, float sb, float er, float eg, float eb);
void lsdl_draw_point(float x, float y, float r, float g, float b);

/* lapis */

int       lapis_init();
void      lapis_deinit();

#endif  /* __LAPIS_H__ */

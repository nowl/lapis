#include "lapis.h"

struct event_data
{
    enum {
        KEY,
        MOUSE_BUTTON,
        MOUSE_MOTION,
        RESIZE,
        QUIT
    } type;
    
    union {
        struct {
            enum {
                UP,
                DOWN
            } type;
            int mod;
            int key;
        } key;
        struct {
            int width;
            int height;
        } resize;
        struct {
            int x;
            int y;
        } mouse_motion;
        struct {
            int type;
            int x;
            int y;
            int button;
        } mouse_button;
    } data;
};

int lsdl_poll_event(struct event_data *event)
{
    SDL_Event e;
    int result = SDL_PollEvent(&e);

    if(!result) return 0;
    
    switch(e.type)
    {
    case SDL_KEYDOWN:
        event->type = KEY;
        event->data.key.type = DOWN;
        event->data.key.mod = e.key.keysym.mod;
        event->data.key.key = e.key.keysym.sym;
        break;
    case SDL_KEYUP:
        event->type = KEY;
        event->data.key.type = UP;
        event->data.key.mod = e.key.keysym.mod;
        event->data.key.key = e.key.keysym.sym;
        break;
    case SDL_VIDEORESIZE:
        event->type = RESIZE;
        event->data.resize.width = e.resize.w;
        event->data.resize.height = e.resize.h;
        break;
    case SDL_MOUSEMOTION:
        event->type = MOUSE_MOTION;
        event->data.mouse_motion.x = e.motion.x;
        event->data.mouse_motion.y = e.motion.y;
        break;
    case SDL_MOUSEBUTTONDOWN:
        event->type = MOUSE_BUTTON;
        event->data.mouse_button.type = DOWN;
        event->data.mouse_button.x = e.button.x;
        event->data.mouse_button.y = e.button.y;
        event->data.mouse_button.button = e.button.button;
        break;
    case SDL_MOUSEBUTTONUP:
        event->type = MOUSE_BUTTON;
        event->data.mouse_button.type = UP;
        event->data.mouse_button.x = e.button.x;
        event->data.mouse_button.y = e.button.y;
        event->data.mouse_button.button = e.button.button;
        break;
    case SDL_QUIT:
        event->type = QUIT;
        break;
    }

    return 1;
}

unsigned int
lsdl_get_tick()
{
    return SDL_GetTicks();
}

static void
setup_screen_params(int flags, int w, int h)
{
    SDL_Surface *screen = SDL_SetVideoMode(w, h, 16, flags);
    if ( !screen )
    {
        LOG("Unable to set video mode: %s\n", SDL_GetError());
        exit(1);
    }

    /* gl setup */

    //glShadeModel( GL_SMOOTH );

    //glCullFace( GL_BACK );
    //glFrontFace( GL_CCW );
    //glEnable( GL_CULL_FACE );

    glClearColor( 0, 0, 0, 0 );
    glViewport( 0, 0, w, h );

    glMatrixMode( GL_PROJECTION );
    glLoadIdentity( );
    glOrtho(0, w, h, 0, 0, 1);
}

void
lsdl_set_video_mode(unsigned int screen_width,
                    unsigned int screen_height,
                    unsigned char fullscreen,
                    unsigned char resizable)
{    
    SDL_GL_SetAttribute( SDL_GL_RED_SIZE, 5 );
    SDL_GL_SetAttribute( SDL_GL_GREEN_SIZE, 5 );
    SDL_GL_SetAttribute( SDL_GL_BLUE_SIZE, 5 );
    SDL_GL_SetAttribute( SDL_GL_DOUBLEBUFFER, 1 );
    
    int flags = SDL_OPENGL;
    
    if(fullscreen)
        flags |= SDL_FULLSCREEN;
    if(resizable)
        flags |= SDL_RESIZABLE;

    setup_screen_params(flags, screen_width, screen_height);

    glDisable(GL_DEPTH_TEST);
    glEnable(GL_TEXTURE_2D);  
}

void
lsdl_fill_rect(float x, float y, float w, float h, float red, float green, float blue)
{
    glColor3f(red, green, blue);

    //glBegin(GL_QUADS);
    glVertex2f(x, y);
    glVertex2f(x+w, y);
    glVertex2f(x+w, y+h);
    glVertex2f(x, y+h);
    //glEnd();
}

void
lsdl_draw_line(float sx, float sy, float ex, float ey, float sr, float sg, float sb, float er, float eg, float eb)
{
    //glBegin(GL_LINES);
    glColor3f(sr, sg, sb);
    glVertex2f(sx, sy);
    glColor3f(er, eg, eb);
    glVertex2f(ex, ey);
    //glEnd();
}

void
lsdl_draw_point(float x, float y, float r, float g, float b)
{
    //glBegin(GL_POINTS);
    glColor3f(r, g, b);
    glVertex2f(x, y);
    //glEnd();
}

#if 0

void
lsdl_draw_image(GLuint texture, float x, float y, float w, float h, float r, float g, float b)
{
    glBindTexture(GL_TEXTURE_2D, texture);

    glColor3f(r, g, b);

    glBegin(GL_QUADS);
    glTexCoord2f(0, 0);
    glVertex2f(x, y);
    glTexCoord2f(1, 0);
    glVertex2f(x+w, y);
    glTexCoord2f(1, 1);
    glVertex2f(x+w, y+h);
    glTexCoord2f(0, 1);
    glVertex2f(x, y+h);
    glEnd();

    glBindTexture(GL_TEXTURE_2D, 0);
}

void
lsdl_draw_text(char *font_name, int pt_size,
               char *text, int r, int g, int b,
               int x, int y)
{
    SDL_Surface *surf = sdl_font_get_surf(font_name, pt_size, text, r, g, b);

    SDL_Surface *corrected_surf = SDL_CreateRGBSurface(0, surf->w, surf->h,
                                                       32,
                                                       surf->format->Rmask,
                                                       surf->format->Gmask,
                                                       surf->format->Bmask,
                                                       surf->format->Amask);
    /* ignore alpha info */
    SDL_SetAlpha(corrected_surf, 0, 0);
    /* XXX: set color key to 0 to mask out the transparent parts*/
    SDL_SetColorKey(corrected_surf, SDL_SRCCOLORKEY, 0);

    if(!corrected_surf)
    {
        WARN("SDL_CreateRGBSurface fail\n");
        return;
    }

    //SDL_Rect source_rect = {0, 0, surf->w, surf->h};
    int res = SDL_BlitSurface(surf, NULL, corrected_surf, NULL);
    if(res != 0)
    {
        SDL_FreeSurface(corrected_surf);
        WARN("SDL_BlitSurface fail: %s\n", SDL_GetError());
        return;
    }

    GLuint texture = opengl_texture_from_surface(corrected_surf);

    SDL_FreeSurface(corrected_surf);

    lsdl_draw_image(texture,
                    x,
                    y,
                    surf->w,
                    surf->h,
                    1.0, 1.0, 1.0);

    glDeleteTextures( 1, &texture);
}

#endif

void
lsdl_prepare_render()
{
    /* Clear the color and depth buffers. */
    glClear(GL_COLOR_BUFFER_BIT);

    /* We don't want to modify the projection matrix. */
    glMatrixMode( GL_MODELVIEW );
    glLoadIdentity( );
}

void
lsdl_flip()
{
    SDL_GL_SwapBuffers();
}

void
lsdl_enable_smooth_lines()
{
    glEnable( GL_LINE_SMOOTH );
    glHint(GL_LINE_SMOOTH_HINT, GL_NICEST);
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
}

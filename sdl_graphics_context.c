#include "lapis.h"

unsigned int
lsdl_get_tick()
{
    return SDL_GetTicks();
}

static void
setup_screen_params(sdl_graphics_context_t *gc, int w, int h)
{
    SDL_Surface *screen = SDL_SetVideoMode(w, h, 16, gc->flags_for_setvideomode);
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
lsdl_set_video_mode(sdl_graphics_context_t* gc,
                    unsigned int screen_width,
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

    gc->flags_for_setvideomode = flags;
    
    setup_screen_params(gc, screen_width, screen_height);

    glDisable(GL_DEPTH_TEST);
    glEnable(GL_TEXTURE_2D);  
}

void
lsdl_resize_internal(int w, int h)
{
    sdl_graphics_context_t *gc = lapis_get_engine()->sdl_driver;
    
    /* SDL hack? to reset the window size have to do SDL_SetVideoMode again? */
    setup_screen_params(gc, w, h);
}

void
lsdl_fill_rect(engine_t *engine, float x, float y, float w, float h, float red, float green, float blue)
{
    glColor3f(red, green, blue);

    glBegin(GL_QUADS);
    glVertex2f(x, y);
    glVertex2f(x+w, y);
    glVertex2f(x+w, y+h);
    glVertex2f(x, y+h);
    glEnd();
}

void
lsdl_draw_image(engine_t *engine, GLuint texture, float x, float y, float w, float h, float brightness)
{
    glBindTexture(GL_TEXTURE_2D, texture);

    glColor3f(brightness, brightness, brightness);

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
lsdl_prepare_render()
{
    /* Clear the color and depth buffers. */
    glClear(GL_COLOR_BUFFER_BIT);

    /* We don't want to modify the projection matrix. */
    glMatrixMode( GL_MODELVIEW );
    glLoadIdentity( );
}

void
lsdl_flip(engine_t * engine)
{
    SDL_GL_SwapBuffers();
}

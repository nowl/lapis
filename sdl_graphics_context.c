#include "lapis.h"

unsigned int
lsdl_get_tick()
{
    return SDL_GetTicks();
}

void
lsdl_set_video_mode(sdl_graphics_context_t* gc,
                    unsigned int screen_width,
                    unsigned int screen_height,
                    unsigned char fullscreen)
{
    SDL_GL_SetAttribute( SDL_GL_RED_SIZE, 5 );
    SDL_GL_SetAttribute( SDL_GL_GREEN_SIZE, 5 );
    SDL_GL_SetAttribute( SDL_GL_BLUE_SIZE, 5 );
    SDL_GL_SetAttribute( SDL_GL_DOUBLEBUFFER, 1 );

    int flags = SDL_OPENGL;
    if(fullscreen)
        flags |= SDL_FULLSCREEN;
    gc->screen = SDL_SetVideoMode(screen_width, screen_height, 16, flags);
    if ( !gc->screen )
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
    glViewport( 0, 0, screen_width, screen_height );

    glMatrixMode( GL_PROJECTION );
    glLoadIdentity( );
    glOrtho(0, screen_width, screen_height, 0, 0, 1);

    glDisable(GL_DEPTH_TEST);
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
lsdl_draw_image(engine_t *engine, SDL_Surface *surf, float x, float y)
{
    SDL_Surface *screen = engine->sdl_driver->screen;

    SDL_Rect dest_rect = {x, y, 0, 0};
    SDL_BlitSurface(surf, NULL, screen, &dest_rect);
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

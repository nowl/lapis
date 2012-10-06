#include "sdl_driver.hpp"
#include "hash.hpp"
#include "engine.hpp"
#include "log.hpp"

SDLDriver::SDLDriver()
{
    if( SDL_Init(SDL_INIT_VIDEO | SDL_INIT_AUDIO) < 0 )
        WARN("Unable to init SDL: %s\n", SDL_GetError());
    
    if( IMG_Init(IMG_INIT_JPG|IMG_INIT_PNG) < 0 )
        WARN("Unable to init IMG: %s\n", IMG_GetError());
    
    if( TTF_Init() < 0 )
        WARN("Unable to init TTF: %s\n", TTF_GetError());
    
    //if( Mix_Init(MIX_INIT_OGG) < 0 )
    //    WARN("Unable to init MIX: %s\n", Mix_GetError());
    
    Mix_AllocateChannels(16);
    if( Mix_OpenAudio(44100, MIX_DEFAULT_FORMAT, 2, 1024) == -1 )
        WARN("Mix_OpenAudio error: %s\n", Mix_GetError());
    
    SDL_EnableKeyRepeat(SDL_DEFAULT_REPEAT_DELAY, SDL_DEFAULT_REPEAT_INTERVAL);

    LOG("SDL initialization successful\n");
}

SDLDriver::~SDLDriver()
{
#ifndef NOCLEANUP
    LOG("shutting down SDL\n");
    
    Mix_CloseAudio();
        
    //Mix_Quit();
    TTF_Quit();
    IMG_Quit();
    SDL_Quit();
#endif
}

std::unique_ptr<SDLDriver> SDLDriver::_instance = nullptr;

const std::unique_ptr<SDLDriver>&
SDLDriver::Instance()
{
    if( !_instance )
        _instance = std::unique_ptr<SDLDriver>(new SDLDriver());
    
    return _instance;
}

unsigned long SDLDriver::getTick() const
{
    return SDL_GetTicks();
}

void SDLDriver::handleEvents()
{
    // message processing loop
    SDL_Event event;
    while (SDL_PollEvent(&event))
    {
        SDLEventPayload payload;
        payload.event = event;
        Message::send(NULL,
                      Engine::UIEVENT_MESSAGE,
                      payload,
                      Message::ASYNC);
    }
}

void SDLDriver::setVideoMode(unsigned int width,
                             unsigned int height,
                             int extra_flags)
{
    SDL_GL_SetAttribute( SDL_GL_RED_SIZE, 5 );
    SDL_GL_SetAttribute( SDL_GL_GREEN_SIZE, 5 );
    SDL_GL_SetAttribute( SDL_GL_BLUE_SIZE, 5 );
    SDL_GL_SetAttribute( SDL_GL_DOUBLEBUFFER, 1 );

    int flags = SDL_OPENGL;
    /*
    if(fullscreen)
        flags |= SDL_FULLSCREEN;
    if(resizable)
        flags |= SDL_RESIZABLE;
    */
    
    _screen = SDL_SetVideoMode(width, height, 16, flags | extra_flags);
    /*
    if ( !screen )
    {
        LOG("Unable to set video mode: %s\n", SDL_GetError());
        exit(1);
    }
    */

    /* gl setup */

    //glShadeModel( GL_SMOOTH );

    //glCullFace( GL_BACK );
    //glFrontFace( GL_CCW );
    //glEnable( GL_CULL_FACE );

    glClearColor( 0, 0, 0, 0 );
    glViewport( 0, 0, width, height );

    glMatrixMode( GL_PROJECTION );
    glLoadIdentity( );
    glOrtho(0, width, height, 0, 0, 1);

    glDisable(GL_DEPTH_TEST);
    //glEnable(GL_TEXTURE_2D);
}


void
SDLDriver::drawImage(GLuint texture, float x, float y, float w, float h, float r, float g, float b)
{
    glEnable(GL_TEXTURE_2D);
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
SDLDriver::preRender()
{
    glClear(GL_COLOR_BUFFER_BIT);

    glMatrixMode( GL_MODELVIEW );
    glLoadIdentity( );
}

void
SDLDriver::postRender()
{
    SDL_GL_SwapBuffers();
}

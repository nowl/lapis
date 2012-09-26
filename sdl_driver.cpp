#include <SDL.h>
#include <SDL_image.h>
#include <SDL_mixer.h>
#include <SDL_ttf.h>

#include "sdl_driver.hpp"
#include "hash.hpp"
#include "log.hpp"

class SDLEventPayload : public Message::IPayload
{
public:
    SDL_Event event;
};

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
    
    LOG("SDL initialization successful\n");
}

SDLDriver::~SDLDriver()
{
    LOG("shutting down SDL\n");
    
    Mix_CloseAudio();
        
    //Mix_Quit();
    TTF_Quit();
    IMG_Quit();
    SDL_Quit();
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
                      Hash::hashString("sdl-event"),
                      payload,
                      Message::ASYNC);
    }
}

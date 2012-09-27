#ifndef __SDL_DRIVER_H__
#define __SDL_DRIVER_H__

#include <memory>

#include <SDL.h>
#include <GL/gl.h>
#include <GL/glu.h>
#include <SDL_image.h>
#include <SDL_mixer.h>
#include <SDL_ttf.h>

#include "message.hpp"

class SDLDriver
{
public:
    static const std::unique_ptr<SDLDriver>& Instance();

    ~SDLDriver();

    void setVideoMode(unsigned int width, unsigned int height, int extra_flags=0);

    unsigned long getTick() const;

    void handleEvents();

private:    
    SDLDriver();

    static std::unique_ptr<SDLDriver> _instance;
    SDL_Surface *_screen;
};

class SDLEventPayload : public Message::IPayload
{
public:
    SDL_Event event;
};

#endif  // __SDL_DRIVER_H__

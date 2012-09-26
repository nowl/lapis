#ifndef __SDL_DRIVER_H__
#define __SDL_DRIVER_H__

#include <memory>

#include "message.hpp"

class SDLDriver
{
public:
    static const std::unique_ptr<SDLDriver>& Instance();

    ~SDLDriver();

    unsigned long getTick() const;

    void handleEvents();

private:    
    SDLDriver();

    static std::unique_ptr<SDLDriver> _instance;
};

class SDLEventPayload;

#endif  // __SDL_DRIVER_H__

#ifndef __ENGINE_HPP__
#define __ENGINE_HPP__

#include <tr1/memory>

class SDLDriver;

class Engine
{
public:
    Engine()
        : _sdlDriver(new SDLDriver())
    {}

    std::unique_ptr<SDLDriver>& getSDLDriver()
    {
        return _sdlDriver;
    }

private:
    std::unique_ptr<SDLDriver> _sdlDriver;
};

#endif  // __ENGINE_HPP__

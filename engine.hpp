#ifndef __ENGINE_HPP__
#define __ENGINE_HPP__

#include <memory>

class SDLDriver;

class Engine
{
public:
    typedef const std::unique_ptr<Engine>& pointer;

    static pointer Instance();

    const std::unique_ptr<SDLDriver>& getSDLDriver()
    {
        return _sdlDriver;
    }

private:
    Engine();

    std::unique_ptr<SDLDriver> _sdlDriver;
    static std::unique_ptr<Engine> _instance;
};

#endif  // __ENGINE_HPP__

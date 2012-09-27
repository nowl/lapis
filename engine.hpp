#ifndef __ENGINE_HPP__
#define __ENGINE_HPP__

#include <memory>

class SDLDriver;

class Engine
{
public:
    Engine();

    const std::unique_ptr<SDLDriver>& getSDLDriver() const;

    unsigned long getTick() const;

    // starts the engine mainloop
    void run();
    void quit();

    bool isRunning() const;

private:
    const std::unique_ptr<SDLDriver>& _sdlDriver;
    bool _isRunning;

    void handleEvents();
};

#endif  // __ENGINE_HPP__

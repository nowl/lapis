#ifndef __ENGINE_HPP__
#define __ENGINE_HPP__

#include <memory>

class SDLDriver;
class ImageLoader;

class Engine
{
public:
    static const char *UIEVENT_MESSAGE;
    static const char *UPDATE_MESSAGE;
    static const char *RENDER_MESSAGE;

    Engine();

    SDLDriver* getSDLDriver();
    ImageLoader* getImageLoader();

    unsigned long getTick() const;

    // starts the engine mainloop
    void run();
    void quit();

    void update(unsigned long gameTick);
    void render(float interpolation);

    bool isRunning() const;

    void setMSPerTick(unsigned int msPerTick);
    void setMaxFrameSkip(unsigned int maxFrameSkip);

private:
    const std::unique_ptr<SDLDriver>& _sdlDriver;
    const std::unique_ptr<ImageLoader> _imageLoader;
    bool _isRunning;
    int _framesPerSecond;
    unsigned int _msPerTick;
    unsigned int _maxFrameSkip;

    void handleEvents();
};

#endif  // __ENGINE_HPP__

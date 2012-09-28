#include <iostream>

#include "engine.hpp"
#include "sdl_driver.hpp"
#include "log.hpp"
#include "payloads_generic.hpp"

const char *Engine::UIEVENT_MESSAGE = "__UI_EVENT__";
const char *Engine::UPDATE_MESSAGE = "__UPDATE_MESSAGE__";
const char *Engine::RENDER_MESSAGE = "__RENDER_MESSAGE__";

Engine::Engine()
    : _sdlDriver(SDLDriver::Instance()),
      _isRunning(false),
      _framesPerSecond(0),
      _msPerTick(1000/20),
      _maxFrameSkip(5)
{}

const std::unique_ptr<SDLDriver>&
Engine::getSDLDriver() const
{
    return _sdlDriver;
}

unsigned long Engine::getTick() const
{
    return getSDLDriver()->getTick();
}

bool Engine::isRunning() const
{
    return _isRunning;
}

void Engine::handleEvents()
{
    return getSDLDriver()->handleEvents();
}

void Engine::run()
{
    LOG("starting engine\n");
    
    _isRunning = true;

    unsigned long next_game_tick = getTick();

	unsigned long fps_counter = 0;
	unsigned long game_tick = 0;
	unsigned long fps_start_time = getTick();
    
    while ( isRunning() )
    {
        handleEvents();        

        unsigned int loops = 0;
        unsigned int tick = getTick();
        while(tick > next_game_tick && loops < _maxFrameSkip)
        {
            update(game_tick++);
            next_game_tick += _msPerTick;
            loops++;

            tick = getTick();
        }

        float interpolation = (float)(tick + _msPerTick - next_game_tick)/_msPerTick;
        render(interpolation);

		++fps_counter;
		if(getTick() - fps_start_time > 1000)
		{
            _framesPerSecond = fps_counter;
			fps_counter = 0;
			fps_start_time = getTick();

            LOG("fps = %d\n", _framesPerSecond);
		}
    }
    
    LOG("stopping engine\n");
}

void Engine::quit()
{
    _isRunning = false;
}

void Engine::setMSPerTick(unsigned int msPerTick)
{
    _msPerTick = msPerTick;
}

void Engine::setMaxFrameSkip(unsigned int maxFrameSkip)
{
    _maxFrameSkip = maxFrameSkip;
}

void Engine::update(unsigned long gameTick)
{
    auto payload = IntPayload(gameTick);
    Message::send(NULL,
                  UPDATE_MESSAGE,
                  payload,
                  Message::ASYNC);
}

void Engine::render(float interpolation)
{
    auto payload = FloatPayload(interpolation);
    Message::send(NULL,
                  RENDER_MESSAGE,
                  payload,
                  Message::ASYNC);
}

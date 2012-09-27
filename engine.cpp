#include <iostream>

#include "engine.hpp"
#include "sdl_driver.hpp"
#include "log.hpp"

Engine::Engine()
    : _sdlDriver(SDLDriver::Instance()),
      _isRunning(false)
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

        fps_start_time = getTick();
        
/*

        int loops = 0;
        unsigned int tick = lsdl_get_tick();
        while(tick > next_game_tick && loops < max_frame_skip)
        {
            engine_update(eng, game_tick++);
            next_game_tick += time_per_tick;
            loops++;

            tick = lsdl_get_tick();
        }

        float interpolation = (tick + time_per_tick - next_game_tick)/time_per_tick;
        engine_render(eng, interpolation);

		++fps_counter;
		if(lsdl_get_tick() - fps_start_time > 1000)
		{
            eng->fps = fps_counter;
			fps_counter = 0;
			fps_start_time = lsdl_get_tick();
            //LOG("fps = %d\n", eng->fps);
		}
    */
    }
    
    LOG("stopping engine\n");
}

void Engine::quit()
{
    _isRunning = false;
}

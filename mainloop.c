#include "lapis.h"

static unsigned int ticks_per_second = 15;
static float time_per_tick;
static unsigned int max_frame_skip = 5;

void
set_ticks_per_second(unsigned int num)
{
    ticks_per_second = num;
    time_per_tick = 1000.0 / num;
}

void
set_max_frame_skip(unsigned int num)
{
    max_frame_skip = num;
}

void
mainloop(engine_t* eng)
{
    engine_start(eng);

    unsigned long next_game_tick = lsdl_get_tick();

	unsigned long fps_counter = 0;
	unsigned long game_tick = 0;
	unsigned long fps_start_time = lsdl_get_tick();

    while (eng->is_running)
    {
        engine_handle_events(eng);

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
    }
}

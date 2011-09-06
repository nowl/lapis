#include "lapis.h"

engine_t *
engine_create()
{
    engine_t* eng = malloc(sizeof(*eng));
    eng->sdl_driver = malloc(sizeof(*eng->sdl_driver));
    eng->is_running = TRUE;
    eng->state = NULL;
	eng->fps = 0;
    return eng;
}

void
engine_destroy(engine_t *eng)
{
    free(eng->sdl_driver);
    free(eng);

    //sdl_font_cleanup();
    //image_loader_cleanup();
    //sound_loader_cleanup();
}

void
engine_switch_state(engine_t *eng, game_state_t* state)
{
    eng->state = state;
}

void
engine_handle_events(engine_t *eng)
{
    // message processing loop
    SDL_Event event;
    while (SDL_PollEvent(&event))
    {
        SDL_Event *eventp = malloc(sizeof(*eventp));
        memcpy(eventp, &event, sizeof(*eventp));
        ref_t *ref = ref_create(eventp);
        message_t *message = message_create(NULL, NULL, "sdl-event", ref);
        message_deliver(message, ASYNC);
        ref_dec(ref);
    }
}

void
engine_update(engine_t *eng, unsigned int ticks)
{
    eng->tick = ticks;
    
    if(eng->state)
        game_state_update(eng, ticks);
}

void
engine_render(engine_t *eng, float interpolation)
{
    if(eng->state)
        game_state_render(eng, interpolation);
}

void engine_start(engine_t *eng) {}
void engine_quit(engine_t *eng) {eng->is_running = FALSE;}

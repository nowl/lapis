#include "lapis.h"

static engine_t *engine = NULL;

int
lapis_init()
{
    random_init();

    if( SDL_Init(SDL_INIT_VIDEO | SDL_INIT_AUDIO) < 0 )
    {
        WARN("Unable to init SDL: %s\n", SDL_GetError());
        goto FAIL;
    }

    if( IMG_Init(IMG_INIT_JPG|IMG_INIT_PNG) < 0 )
    {
        WARN("Unable to init IMG: %s\n", IMG_GetError());
        goto FAIL;
    }

    if( TTF_Init() < 0 )
    {
        WARN("Unable to init TTF: %s\n", TTF_GetError());
        goto FAIL;
    }
    
    LOG("SDL initialization successful\n");

    /* initialize engine */
    
    engine = engine_create();
   
    return 0;

FAIL:
    return 1;
}

engine_t *lapis_get_engine()
{
    return engine;
}

void
lapis_mainloop()
{
    mainloop(engine);
}

void
lapis_deinit()
{
    image_render_set_cleanup();

    if(engine)
        engine_destroy(engine);

    TTF_Quit();
    IMG_Quit();
    SDL_Quit();
}

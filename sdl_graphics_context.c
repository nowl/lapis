#include "lapis.h"

#define MAX_DIRTY_RECTS 512

unsigned int
lsdl_get_tick()
{
    return SDL_GetTicks();
}

void
lsdl_set_video_mode(sdl_graphics_context_t* gc,
                    unsigned int screen_width,
                    unsigned int screen_height,
                    unsigned char fullscreen)
{
    int flags = SDL_SWSURFACE|SDL_DOUBLEBUF;
    if(fullscreen)
        flags |= SDL_FULLSCREEN;
    gc->screen = SDL_SetVideoMode(screen_width, screen_height, 16, flags);
    if ( !gc->screen )
    {
        LOG("Unable to set video mode: %s\n", SDL_GetError());
        exit(1);
    }
    
    gc->dirty_rects = malloc(sizeof(*gc->dirty_rects) * MAX_DIRTY_RECTS);
    gc->dirty_rects_i = 0;
    gc->erase_rects = malloc(sizeof(*gc->erase_rects) * MAX_DIRTY_RECTS);
    gc->erase_rects_i = 0;

	gc->full_screen_update = 0;
}

void
lsdl_fill_rect(engine_t *engine, int x, int y, int w, int h, int red, int green, int blue)
{
    SDL_Surface *screen = engine->sdl_driver->screen;

    SDL_Rect dest = {x, y, w, h};

    SDL_FillRect(screen, &dest, SDL_MapRGB(screen->format, red, green, blue));

    lsdl_dirty_rect(engine, x, y, w, h);
}

void
lsdl_dirty_rect(engine_t *engine, int x, int y, int w, int h)
{

    SDL_Rect *dirty_rects = engine->sdl_driver->dirty_rects;
    int dirty_rects_i = engine->sdl_driver->dirty_rects_i;

    /* do some manual clipping */
    if(x < 0) x = 0;
    if(x >= 800) x = 799;
    if(x+w >= 800) w = 799-x;
    if(y < 0) y = 0;
    if(y >= 600) y = 599;
    if(y+h >= 600) h = 599-y;

    if(dirty_rects_i < MAX_DIRTY_RECTS)
    {
        dirty_rects[dirty_rects_i].x = x;
        dirty_rects[dirty_rects_i].y = y;
        dirty_rects[dirty_rects_i].w = w;
        dirty_rects[dirty_rects_i].h = h;
		++engine->sdl_driver->dirty_rects_i;
    } else {
		engine->sdl_driver->full_screen_update = 1;
	}
}

void
lsdl_draw_image(engine_t *engine, SDL_Surface *surf, int x, int y)
{
    SDL_Surface *screen = engine->sdl_driver->screen;

    SDL_Rect dest_rect = {x, y, 0, 0};
    SDL_BlitSurface(surf, NULL, screen, &dest_rect);

    lsdl_dirty_rect(engine, x, y, surf->w, surf->h);
}

static void
erase_rects(engine_t *engine)
{
    // erase screen

    SDL_Surface *screen = engine->sdl_driver->screen;
    SDL_FillRect(screen, NULL, SDL_MapRGB(screen->format, 0, 0, 0));

    // copy erased rects

    memcpy(engine->sdl_driver->erase_rects,
		   engine->sdl_driver->dirty_rects,
		   engine->sdl_driver->dirty_rects_i * sizeof(*engine->sdl_driver->dirty_rects));
    engine->sdl_driver->erase_rects_i = engine->sdl_driver->dirty_rects_i;
}

void
lsdl_flip(engine_t * engine)
{
    /* Dirty_rects holds the new image locations and erase_rects holds the previous frames
     * locations. Both should be updated and then dirty_rects should be copied over to erase_rects,
     * dirty_rects should then be erased.
     */

    SDL_Surface *screen = engine->sdl_driver->screen;

	if(!engine->sdl_driver->full_screen_update)
	{

		SDL_UpdateRects(screen, engine->sdl_driver->dirty_rects_i, engine->sdl_driver->dirty_rects);
		SDL_UpdateRects(screen, engine->sdl_driver->erase_rects_i, engine->sdl_driver->erase_rects);

		erase_rects(engine);

		engine->sdl_driver->dirty_rects_i = 0;
	} else {
        SDL_Flip(screen);
        SDL_FillRect(screen, NULL, SDL_MapRGB(screen->format, 0, 0, 0));

		engine->sdl_driver->full_screen_update = 0;
	}
}

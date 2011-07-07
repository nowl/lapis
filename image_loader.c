#include "lapis.h"

/* quick cache will contain the previously loaded "raw" image */
static char *quick_cache_filename = NULL;
static SDL_Surface *quick_cache = NULL;

static struct {
    /* reference to the surface object returned from loading a surface through SDL_image */
    SDL_Surface *surface;

    /* a copy of a string representing the alias of this image */
    char *alias;
} *cache = NULL;
static size_t cache_len = 0;
static size_t cache_cap = 0;

int
image_loader_load(char *alias,
                  char *filename,
                  int x,
                  int y,
                  int width,
                  int height)
{
    SDL_Surface *raw;

    /* check quick cache */
    if(!quick_cache_filename || strcmp(filename, quick_cache_filename) != 0) {
        raw = IMG_Load(filename);
        if(!raw)
        {
            LOG("IMG_Load: %s\n", IMG_GetError());
            return -1;
        }

        /* free if necessary */
        if(quick_cache_filename)
            free(quick_cache_filename);
        if(quick_cache)
            SDL_FreeSurface(quick_cache);

        quick_cache = raw;
        quick_cache_filename = strdup(filename);
        LOG("quick caching \"%s\"\n", filename);
    } else {
        raw = quick_cache;
    }

    SDL_Surface *image = SDL_CreateRGBSurface(0, width, height,
                                              raw->format->BitsPerPixel,
                                              raw->format->Rmask,
                                              raw->format->Gmask,
                                              raw->format->Bmask,
                                              raw->format->Amask);
    /* ignore alpha info */
    SDL_SetAlpha(image, 0, 0);
    /* XXX: set color key to 0 to mask out the transparent parts*/
    SDL_SetColorKey(image, SDL_SRCCOLORKEY, 0);

    if(!image)
    {
        SDL_FreeSurface(raw);
        free(quick_cache_filename);
        quick_cache_filename = NULL;
        quick_cache = NULL;
        LOG("SDL_CreateRGBSurface fail\n");
        return -1;
    }

    SDL_Rect source_rect = {x, y, width, height};

    int r = SDL_BlitSurface(raw, &source_rect, image, NULL);
    if(r != 0)
    {
        SDL_FreeSurface(image);
        SDL_FreeSurface(raw);
        free(quick_cache_filename);
        quick_cache_filename = NULL;
        quick_cache = NULL;
        LOG("SDL_BlitSurface fail: %s\n", SDL_GetError());
        return -1;
    }
    
    int index = cache_len++;
    cache = memory_grow_to_size(cache,
                                sizeof(*cache),
                                &cache_cap,
                                cache_len);

    cache[index].surface = image;
    cache[index].alias = strdup(alias);

    LOG("cached cropped surface with alias \"%s\"\n", alias);

    return 0;
}

SDL_Surface *
image_loader_get(char *alias)
{
    int i;
    for(i=0; i<cache_len; i++)
    {
        if( strcmp(cache[i].alias, alias) == 0 )
        {
            LOG("grabbed image %p for alias \"%s\"\n", &cache[i], alias);
            return cache[i].surface;
        }
    }
    
    ERROR("couldn't find cached image for alias \"%s\"\n", alias);
    return NULL;
}

void
image_loader_cleanup()
{
    int i;
    for(i=0; i<cache_len; i++)
    {
        SDL_FreeSurface(cache[i].surface);
        free(cache[i].alias);
    }

    free(cache);
    cache = NULL;
    cache_len = 0;
    cache_cap = 0;
}

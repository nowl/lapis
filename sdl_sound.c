#include "lapis.h"

struct sound_cache {
    Mix_Chunk *sound;

    /* a copy of a string representing the alias of this sound */
    char *alias;
};

static struct sound_cache** cache = NULL;
size_t cache_len = 0;
size_t cache_cap = 0;

int
sdl_sound_load(char *file, char *alias)
{
    struct sound_cache *new_entry = malloc(sizeof(*new_entry));
    new_entry->sound = Mix_LoadWAV(file);
    new_entry->alias = strdup(alias);
    
    cache = memory_grow_to_size(cache,
                                sizeof(*cache),
                                &cache_cap,
                                cache_len + 1);
    cache[cache_len] = new_entry;
    cache_len++;

    LOG("cached sound from file \"%s\"\n", file);
    
    return 0;
}

void
sdl_sound_play(char *alias)
{
    int i;
    for(i=0; i<cache_len; i++)
    {
        if( strcmp(cache[i]->alias, alias) == 0 )
        {
            Mix_PlayChannel(-1, cache[i]->sound, 0);
            break;
        }
    }
}


void
sound_loader_cleanup()
{
    int i;
    for(i=0; i<cache_len; i++)
    {
        struct sound_cache *entry = cache[i];
        
        Mix_FreeChunk(entry->sound);
        free(entry->alias);
        free(entry);
    }
    
    free(cache);
    cache = NULL;
    cache_len = 0;
}

#include "lapis.h"

struct keyframe {
    GLuint texture;
    unsigned int num_ticks;
};

static struct
{
    char *name;                    /* the name of this image set */
    struct keyframe **frames;      /* the array of keyframes */
    size_t num_frames;             /* the actual number of frames */
    size_t cap_frames;             /* the maximum number of frames allocated */
} *set = NULL;
static size_t set_len = 0;
static size_t set_cap = 0;

void
image_render_set_create(char *name)
{
    int i= set_len++;
    set = memory_grow_to_size(set,
                              sizeof(*set),
                              &set_cap,
                              set_len);

    set[i].name = strdup(name);
    set[i].num_frames = 0;
    set[i].cap_frames = 0;
    set[i].frames = NULL;
}

static int
get_set(char *name)
{
    int i;
    for(i=0; i<set_len; i++)
    {
        if( strcmp(set[i].name, name) == 0 )
            return i;
    }
    
    return -1;
}

/* TODO: do we need a function to reset and image set? */

void
image_render_set_add(char *name, char *image_name, int num_ticks)
{
    int ind = get_set(name);
    if(ind < 0)
    {
        ERROR("attempt to add to nonexistant render_set \"%s\"\n", name);
        return;
    }
    
    set[ind].frames = 
        memory_grow_to_size(set[ind].frames,
                            sizeof(*set[ind].frames),
                            &set[ind].cap_frames,
                            set[ind].num_frames + 1);

    GLuint texture = image_loader_get(image_name);
    struct keyframe *frame = malloc(sizeof(*frame));
    frame->texture = texture;
    frame->num_ticks = num_ticks;
    LOG("assigning texture at id %d\n", texture);
    set[ind].frames[set[ind].num_frames++] = frame;
    LOG("adding %d frames of %s to %s\n", num_ticks, image_name, name);
}

void
image_render_set_cleanup()
{
    int i;
    for(i=0; i<set_len; i++)
    {
        int j;
        for(j=0; j<set[i].num_frames; j++)
            free(set[i].frames[j]);

        free(set[i].frames);
        free(set[i].name);
    }

    free(set);
    set = NULL;
    set_len = 0;
    set_cap = 0;
}

GLuint
image_render_set_get_image(char *name, int cur_tick)
{
    int ind = get_set(name);
    if(ind < 0)
    {
        ERROR("attempting to grab an image from a nonexistant render_set \"%s\"\n", name);
        return 0;
    }

    GLuint result;

    /* hack to skip loop if only one frame */
    if(set[ind].num_frames > 1)
    {
        int frame = 0;
        int skip_frames = 0;

        while(cur_tick >= 0)
        {
            cur_tick -= set[ind].frames[frame]->num_ticks;
            skip_frames++;
        }

        --skip_frames;

        frame = (frame + skip_frames) % set[ind].num_frames;
        
        result = set[ind].frames[frame]->texture;
    } else {
        result = set[ind].frames[0]->texture;
    }

    return result;
}

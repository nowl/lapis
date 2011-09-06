#include "lapis.h"

ref_t *
ref_create(void * data)
{
    ref_t *ref = malloc(sizeof(*ref));
    ref->count = 1;
    ref->data = data;
    return ref;
}

void
ref_inc(ref_t *ref)
{
    ref->count++;
}

void
ref_dec(ref_t *ref)
{
    if(--ref->count == 0)
    {
        if(ref->data)
            free(ref->data);
        free(ref);
    }   
}

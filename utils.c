#include "lapis.h"

void *
memory_grow_to_size(void *mem_p,
                    size_t per_block,
                    size_t *mem_cap_ip,
                    size_t mem_size_i)
{
    /* null memory block */
    if(!mem_p) {
        *mem_cap_ip = mem_size_i;
        return malloc(mem_size_i * per_block);
    }

    /* if need more memory then double cap until we have it */
    if(mem_size_i > *mem_cap_ip) {
        while(mem_size_i > *mem_cap_ip) {
            *mem_cap_ip *= 2;
        }

        mem_p = realloc(mem_p, *mem_cap_ip * per_block);
    }

    return mem_p;
}

static unsigned long
jenkins_hash_char(void* key)
{
    unsigned long hash = 0;
    char *str = key;
    char c;

    //int loop = 0;
    //while (((c = *str++) != 0) && (loop++ < MAX_LOOPS)) {
    while ((c = *str++) != 0) {
        hash += c;
        hash += (hash << 10);
        hash ^= (hash >> 6);
    }
    hash += (hash << 3);
    hash ^= (hash >> 11);
    hash += (hash << 15);
 
    return hash;
}

unsigned long
lapis_hash(char *type)
{
    return jenkins_hash_char(type);
}

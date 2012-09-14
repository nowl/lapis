#ifndef __ENTITY_H__
#define __ENTITY_H__

#include "uthash.h"
#include "utlist.h"

struct entity
{
    char *name;
    struct entity_meta *meta;
    
    struct entity *next, *prev;
};

struct entity_meta
{
    char *key;
    int   type;
    void *value;

    UT_hash_handle hh;
};

struct entity *create_entity(char *name);
void           destroy_entity(struct entity *e);

void entity_set_meta(struct entity *e, 
                     char *key, 
                     int type,
                     void *val);

struct entity_meta * entity_get_meta(struct entity *e,
                                     char *key);
    
void entity_cleanup_mempools();

#endif  /* __ENTITY_H__ */

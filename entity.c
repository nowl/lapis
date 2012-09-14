#include "lapis.h"

/* list of all entities */
static struct entity *entities = NULL;

/* mempools */
static struct mempool *entities_mp = NULL;
static struct mempool *entity_meta_mp = NULL;

struct entity *
create_entity(char *name)
{
    /* create */
    if( !entities_mp)
        entities_mp = mp_create(sizeof(struct entity), MEMPOOL_DEFAULT_SIZE);

    struct entity *ent = mp_alloc(entities_mp);
    
    /* add to global entity list */
    DL_APPEND(entities, ent);

    ent->name = strdup(name);
    ent->meta = NULL;

    return ent;
}

void
destroy_entity(struct entity *e)
{
    free(e->name);

    /* remove from list first */
    DL_DELETE(entities, e);

    /* free hash names */
    struct entity_meta *m = e->meta;
    for(; m; m = m->hh.next)
        free(m->key);

    /* free all of the associated metavalues */
    HASH_CLEAR(hh, e->meta);

    /* free memory */
    mp_free(entities_mp, e);
}

void
entity_set_meta(struct entity *e, 
                char *key, 
                int type,
                void *val)
{
    if( !entity_meta_mp)
        entity_meta_mp = mp_create(sizeof(struct entity_meta), MEMPOOL_DEFAULT_SIZE);

    struct entity_meta * meta = mp_alloc(entity_meta_mp);
    meta->key = strdup(key);
    meta->type = type;
    meta->value = val;

    /* hash this meta value */
    HASH_ADD_KEYPTR(hh, e->meta, meta->key, strlen(meta->key), meta);
}

struct entity_meta *
entity_get_meta(struct entity *e,
                char *key)
{
    struct entity_meta *meta;
    HASH_FIND_STR(e->meta, key, meta);
    return meta;
}

void                     
entity_cleanup_mempools()
{
    mp_destroy(entities_mp);
    mp_destroy(entity_meta_mp);
}

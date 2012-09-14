#include "lapis.h"

/* mempools */
static struct mempool *components_mp = NULL;
static struct mempool *responder_lookup_mp = NULL;
static struct mempool *component_list_mp = NULL;

struct responder_lookup
{
    int type;
    struct component_list *component_list;

    UT_hash_handle hh;
};

struct responder_lookup * responder_lookup = NULL;

struct component *
create_component(char *name)
{
    /* create */
    if( !components_mp)
        components_mp = mp_create(sizeof(struct component),
                                  MEMPOOL_DEFAULT_SIZE);

    struct component *comp = mp_alloc(components_mp);
    
    comp->name = strdup(name);

    return comp;
}

void
component_add_responder(struct component *comp, int type)
{
    /* create the responder lookup hash if it doesn't exist */
    if( !responder_lookup_mp)
        responder_lookup_mp = mp_create(sizeof(struct responder_lookup),
                                        MEMPOOL_DEFAULT_SIZE);
    
    /* see if this responder lookup exists for this responder type*/
    struct responder_lookup *lookup;
    HASH_FIND_INT(responder_lookup, &type, lookup);
    if(! lookup)
    {
        /* create a new component list and add it to the hash table */
        lookup = mp_alloc(responder_lookup_mp);
        lookup->type = type;
        lookup->component_list = NULL;
        HASH_ADD_INT(responder_lookup, type, lookup);
    } else {
        /* check if this component is already in the list */
        struct component_list *cl = lookup->component_list;
        if(cl)
        {
            struct component c = cl->component;
            for(; cl; cl = cl->next)
            {
                c = cl->component;
                if( strcmp(c.name, comp->name) == 0 &&
                    c.cb == comp->cb)
                    return;
            }
        }
    }
    
    /* append this component to the list */
    if( !component_list_mp)
        component_list_mp = mp_create(sizeof(struct component_list),
                                      MEMPOOL_DEFAULT_SIZE);
    
    struct component_list *cl = mp_alloc(component_list_mp);
    cl->component = *comp;
    DL_APPEND(lookup->component_list, cl);
}

struct component_list *
component_responder_lookup(int type)
{
    struct responder_lookup *lookup;
    HASH_FIND_INT(responder_lookup, &type, lookup);

    return lookup->component_list;
}

void
destroy_component(struct component *c)
{
    /* remove instances from responder lookup */
    struct responder_lookup *rl = responder_lookup;
    for(; rl; rl = rl->hh.next)
    {
        struct component_list *head = rl->component_list, *cl, *tmp;
        if(head)
        {            
            DL_FOREACH_SAFE(head, cl, tmp)
            {
                struct component c2 = cl->component;
                if( strcmp(c2.name, c->name) == 0 && c2.cb == c->cb)
                {
                    DL_DELETE(head, cl);
                    break;;
                }
            }
        }
    }

    free(c->name);

    /* free memory */
    mp_free(components_mp, c);
}

void
component_cleanup_mempools()
{
    HASH_CLEAR(hh, responder_lookup);
    
    mp_destroy(components_mp);
    mp_destroy(responder_lookup_mp);
    mp_destroy(component_list_mp);
}

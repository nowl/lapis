#include "lapis.h"

game_state_t *
game_state_create(int id)
{
    game_state_t *gs = malloc(sizeof(*gs));
    gs->id = id;
    gs->num_render_levels = 1;
    gs->objects = NULL;
    gs->bcast_recvrs = NULL;
                                      
    return gs;
}

void
game_state_destroy(game_state_t *state)
{
    free(state);
}

void
game_state_append_object(game_state_t *gs,
                         game_object_t *obj)
{
    aatree_node_t *obj_node = aatree_create(obj->name, obj, 0);
    gs->objects = aatree_insert(gs->objects, obj_node);
}

game_object_t*
game_state_remove_object(game_state_t *gs, game_object_t *obj)
{
    aatree_node_t *n = aatree_find(gs->objects, obj->name);
    if(!n) return NULL;

    gs->objects = aatree_delete(gs->objects, n);
    game_object_t *go = n->data;

    /* remove object from bcast recvrs also */
    list_t *a = list_first(gs->bcast_recvrs);
    while(a)
    {
        list_t *t = a->next;
        
        bcast_recvr_t *br = a->data;
        if(br->obj == obj)
        {
            free(br);
            gs->bcast_recvrs = list_remove(a);
            list_destroy(a);
        }
        
        a = t;
    }

    return go;
}

void
game_state_render(engine_t *eng, float interpolation)
{
    game_state_t *gs = eng->state;

    lsdl_free_font_surfaces();

    glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT);
    
    int rl;                     /* render level */
    for(rl=0; rl<gs->num_render_levels; rl++)
    {
        aatree_node_t *n = aatree_first(gs->objects);
        for(; n; n = aatree_next(n))
        {
            game_object_t *object = n->data;
            if(object && object->render_level == rl)
            {
                lsdl_pre_render(eng);
                
                switch(object->render_callback.type)
                {
                case C_FUNC:
                    object->render_callback.cb.c_func(eng, object, interpolation);
                    break;
                case SCRIPT_FUNC:
                    //eng->script_render_call(object->render_callback.cb.script_func, eng, object, interpolation);
                    break;
                case NONE:
                    break;
                default:
                    assert(FALSE);
                }

                lsdl_post_render(eng);
            }
        }
    }

    lsdl_flip(eng);
}

void
game_state_update(engine_t *eng, unsigned int ticks)
{
    game_state_t *gs = eng->state;

    aatree_node_t *n = aatree_first(gs->objects);
    for(; n; n = aatree_next(n))
    {
        game_object_t *object = n->data;
        if(object)
        {
            game_object_process_messages(object);

            switch(object->update_callback.type)
            {
            case C_FUNC:
                object->update_callback.cb.c_func(eng, object, ticks);
                break;
            case SCRIPT_FUNC:
                //eng->script_update_call(object->update_callback.cb.script_func, eng, object, ticks);
                break;
            case NONE:
                break;
            default:
                assert(FALSE);
            }
        }
    }
}

void
game_state_append_bcast_recvr(game_state_t *state, game_object_t *obj, char *name)
{
    bcast_recvr_t *br = malloc(sizeof(*br));
    br->obj = obj;
    br->hash = lapis_hash(name);
    list_t *le = list_create(br);
    state->bcast_recvrs = list_append(state->bcast_recvrs, le);
}

void
game_state_deliver_message_sync(game_state_t *state, message_t *message)
{
    if(message->receiver)
        game_object_append_message(message->receiver, message);
    else
    {
        /* this is a broadcast message, look for listeners for given
         * type */
        unsigned long type = message->type;

        list_t *a;
        for(a = list_first(state->bcast_recvrs); a; a = a->next)
        {
            bcast_recvr_t *bcast_recv = a->data;
            if(bcast_recv->hash == type)
            {
                /* this object is listening for these */
                game_object_append_message(bcast_recv->obj, message);
            }
        }
    }
}

void
game_state_deliver_message_async(game_state_t *state, message_t *message)
{
    if(message->receiver)
    {
        game_object_recv_mes(message->receiver, message);
    }
    else
    {
        /* this is a broadcast message, look for listeners for given
         * type */
        unsigned long type = message->type;

        list_t *a;
        for(a = list_first(state->bcast_recvrs); a; a = a->next)
        {
            bcast_recvr_t *bcast_recv = a->data;
            if(bcast_recv->hash == type)
            {
                /* this object is listening for these */
                int result = game_object_recv_mes(bcast_recv->obj, message);
                if(result != 0)
                    break;
            }
        }
    }

    message_destroy(message);
}

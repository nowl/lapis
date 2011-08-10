#include "lapis.h"

game_state_t *
game_state_create(int id)
{
    game_state_t *gs = malloc(sizeof(*gs));
    gs->id = id;
    gs->objects = NULL;
    gs->bcast_recvrs_len = 0;
    gs->bcast_recvrs_cap = 0;
    gs->bcast_recvrs = NULL;
                                      
    return gs;
}

void
game_state_destroy(game_state_t *state)
{
    if(state->bcast_recvrs) free(state->bcast_recvrs);
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
    return n->data;

    /* TODO: remove object from bcast recvrs also */
}

void
game_state_render(engine_t *eng, float interpolation)
{
    game_state_t *gs = eng->state;

    lsdl_free_font_surfaces();

    lsdl_prepare_render();

    aatree_node_t *n = aatree_first(gs->objects);
    for(; n; n = aatree_next(n))
    {
        game_object_t *object = n->data;
        if(object)
        {
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
    state->bcast_recvrs = memory_grow_to_size(state->bcast_recvrs,
                                              sizeof(*state->bcast_recvrs),
                                              &state->bcast_recvrs_cap,
                                              state->bcast_recvrs_len + 1);
    state->bcast_recvrs[state->bcast_recvrs_len].obj = obj;
    state->bcast_recvrs[state->bcast_recvrs_len].hash = lapis_hash(name);
    state->bcast_recvrs_len++;
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

        int i;
        for(i=0; i<state->bcast_recvrs_len; i++)
        {
            bcast_recvr_t *bcast_recv = &state->bcast_recvrs[i];
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

        int i;
        for(i=0; i<state->bcast_recvrs_len; i++)
        {
            bcast_recvr_t *bcast_recv = &state->bcast_recvrs[i];
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

#include "lapis.h"

game_state_t *
game_state_create(int id)
{
    game_state_t *gs = malloc(sizeof(*gs));
    gs->id = id;
    gs->objects_len = 0;
    gs->objects_cap = 0;
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
    if(state->objects) free(state->objects);
    free(state);
}

void
game_state_append_object(game_state_t *gs,
                         game_object_t *obj)
{
    gs->objects = memory_grow_to_size(gs->objects,
                                      sizeof(*gs->objects),
                                      &gs->objects_cap,
                                      gs->objects_len + 1);
    gs->objects[gs->objects_len] = obj;
    gs->objects_len++;
}

game_object_t*
game_state_remove_object(game_state_t *gs, game_object_t *obj)
{
    /* TODO: for game_state_remove_object just set the position to
     * NULL and then check if there are enough empty objects to
     * warrant a rebuild of the object list. Perhaps number of nulls >
     * 1/2 the objects_len. */    

    return NULL;
}

void
game_state_render(engine_t *eng, float interpolation)
{
    game_state_t *gs = eng->state;

    //lsdl_free_font_surfaces();

    int i;
    for(i=0; i<gs->objects_len; i++)
    {
        game_object_t *object = gs->objects[i];
        if(object)
        {
/*
            struct render_callback *callback = render_callback_list_SLL_NEXT(&object->render_callbacks, NULL);
            while(callback) {

                switch(callback->type)
                {
                case C_FUNC:
                    callback->cb.c_func(eng, object, interpolation);
                    break;
                case SCRIPT_FUNC:
                    eng->script_render_call(callback->cb.script_func, eng, object, interpolation);
                    break;
                default:
                    assert(FALSE);
                }
                
                // go to the next one
                callback = render_callback_list_SLL_NEXT(&object->render_callbacks, callback);
            }
*/
            /*
              if(object->image)
              black_sdl_draw_image(eng, object->image, object->screenx, object->screeny);
            */
            
//            entry = game_object_list_SLL_NEXT(&gs->objects, entry);
//        }
        
        }
    }
    
    lsdl_flip(eng);
}

void
game_state_update(engine_t *eng, unsigned int ticks)
{
    game_state_t *gs = eng->state;

    int i;
    for(i=0; i<gs->objects_len; i++)
    {
        game_object_t *object = gs->objects[i];
        if(object)
        {
            /*

            // process message list
            struct message *message = message_list_SLL_NEXT(&object->message_list, NULL);
            while(message) {
                message->callback_func(message->sender, message->receiver);

                message = message_list_SLL_NEXT(&object->message_list, message);
            }

            // clear message list
            game_object_clear_messages(object);

            // process update callbacks
            
            struct update_callback *callback = update_callback_list_SLL_NEXT(&object->update_callbacks, NULL);
            while(callback) {

                // call the update function, it is the responsibility of the update function to continue popping events
                // as it needs them and to even exhaust them all if they are applicable to that update function
                
                switch(callback->type)
                {
                case C_FUNC:
                    callback->cb.c_func(eng, object, ticks);
                    break;
                case SCRIPT_FUNC:
                    eng->script_update_call(callback->cb.script_func, eng, object, ticks);
                    break;
                default:
                    assert(FALSE);
                }
                
                // go to the next one
                callback = update_callback_list_SLL_NEXT(&object->update_callbacks, callback);
            }
            
            entry = game_object_list_SLL_NEXT(&gs->objects, entry);
            */
        }
    }
}

void
game_state_append_bcast_recvr(game_state_t *state, game_object_t *obj, unsigned long hash)
{
    state->bcast_recvrs = memory_grow_to_size(state->bcast_recvrs,
                                              sizeof(*state->bcast_recvrs),
                                              &state->bcast_recvrs_cap,
                                              state->bcast_recvrs_len + 1);
    state->bcast_recvrs[state->bcast_recvrs_len].obj = obj;
    state->bcast_recvrs[state->bcast_recvrs_len].hash = hash;
    state->bcast_recvrs_len++;
}

void
game_state_deliver_message_sync(game_state_t *state, message_t message)
{
    game_object_append_message(message.receiver, message);
}

void
game_state_deliver_message_async(game_state_t *state, message_t message)
{
    if(message.receiver)
    {
        game_object_recv_mes(message.receiver, message);
    }
    else
    {
        /* this is a broadcast message, look for listeners for given
         * type */
        unsigned long type = message.type;

        int i;
        for(i=0; i<state->bcast_recvrs_len; i++)
        {
            bcast_recvr_t *bcast_recv = &state->bcast_recvrs[i];
            if(bcast_recv->hash == type)
            {
                /* this object is listening for these */
                int result = game_object_recv_mes(bcast_recv->obj,
                                                  message);
                if(result != 0)
                    break;
            }
        }        
    }
}

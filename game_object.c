#include "lapis.h"

static unsigned long global_id_counter = 0;

static game_object_t **objects = NULL;
static size_t objects_len = 0;
static size_t objects_cap = 0;

game_object_t *
game_object_create(void *data)
{
    game_object_t *obj = malloc(sizeof(*obj));
    obj->id = global_id_counter++;
    obj->data = data;
	obj->image = NULL;
	obj->screenx = 0;
	obj->screeny = 0;
    obj->update_callback.type = NONE;
    obj->render_callback.type = NONE;
    obj->recv_callback = NULL;
    obj->messages = NULL;
    obj->messages_len = 0;
    obj->messages_cap = 0;

    /* cache object */
    objects = memory_grow_to_size(objects,
                                  sizeof(*objects),
                                  &objects_cap,
                                  objects_len + 1);
    objects[objects_len] = obj;
    objects_len++;

    return obj;
}

static game_object_t *
remove_obj_by_id(int id)
{
    int i;
    for(i=0; i<objects_len; i++)
    {
        if(objects[i] && (objects[i]->id == id))
        {
            game_object_t *obj = objects[i];
            objects[i] = NULL;
            return obj;
        }
    }

    return NULL;
}

void
game_object_destroy(engine_t *eng, game_object_t *go)
{
    // TODO: ensure object is removed from all states
    game_state_remove_object(eng->state, go);

    if(go->render_callback.type == SCRIPT_FUNC)
        free(go->render_callback.cb.script_func);
    if(go->update_callback.type == SCRIPT_FUNC)
        free(go->update_callback.cb.script_func);
    
    remove_obj_by_id(go->id);
    free(go);
}

game_object_t *
game_object_get(int id)
{
    int i;
    for(i=0; i<objects_len; i++)
        if(objects[i] && (objects[i]->id == id))
            return objects[i];

    return NULL;
}

game_object_t *
game_object_remove(game_object_t *obj)
{
    return remove_obj_by_id(obj->id);
}

game_object_t *
game_object_remove_by_id(int id)
{
    return remove_obj_by_id(id);
}

void
game_object_set_recv_callback_c_func(game_object_t *obj,
                                     recv_callback_fn callback)
{
    obj->recv_callback = callback;
}

void
game_object_set_update_callback_c_func(game_object_t *obj,
                                       const game_object_update_fn callback)
{
    obj->update_callback.type = C_FUNC;
    obj->update_callback.cb.c_func = callback;
}

void
game_object_set_update_callback_script_func(game_object_t *obj,
                                            char *callback)
{
    obj->update_callback.type = SCRIPT_FUNC;
    obj->update_callback.cb.script_func = strdup(callback);
}

void
game_object_set_render_callback_c_func(game_object_t *obj,
                                       const game_object_render_fn callback)
{
    obj->render_callback.type = C_FUNC;
    obj->render_callback.cb.c_func = callback;
}

void
game_object_set_render_callback_script_func(game_object_t *obj,
                                            char * callback)
{
    obj->render_callback.type = SCRIPT_FUNC;
    obj->render_callback.cb.script_func = strdup(callback);
}

void
game_object_append_message(game_object_t *obj,
                           message_t mes)
{
    obj->messages =
        memory_grow_to_size(obj->messages,
                            sizeof(*obj->messages),
                            &obj->messages_cap,
                            obj->messages_len + 1);
    obj->messages[obj->messages_len] = mes;
    obj->messages_len++;
}

void
game_object_clear_messages(game_object_t *obj)
{
    free(obj->messages);
    obj->messages = NULL;
    obj->messages_len = 0;
    obj->messages_cap = 0;
}

int
game_object_recv_mes(game_object_t *obj, message_t mes)
{
    if(obj->recv_callback)
        return (obj->recv_callback)(obj, mes);

    return 0;
}

void
game_object_process_messages(game_object_t *obj)
{
    int i=0;
    for(i=0; i<obj->messages_len; i++)
        game_object_recv_mes(obj, obj->messages[i]);
    game_object_clear_messages(obj);
}

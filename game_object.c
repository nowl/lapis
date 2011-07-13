#include "lapis.h"

static unsigned long global_id_counter = 0;

static game_object_t **objects = NULL;
static size_t objects_len = 0;
static size_t objects_cap = 0;

game_object_t *
game_object_create(unsigned int type, void *data)
{
    game_object_t *obj = malloc(sizeof(*obj));
    obj->id = global_id_counter++;
    obj->type = type;
    obj->data = data;
	obj->image = NULL;
	obj->screenx = 0;
	obj->screeny = 0;
    obj->update_callback = NULL;
    obj->render_callback = NULL;
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

    if(go->update_callback)
        free(go->update_callback);
    if(go->render_callback)
        free(go->render_callback);
    
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
    update_callback_t *cb = malloc(sizeof(*cb));
    cb->type = C_FUNC;
    cb->cb.c_func = callback;
    obj->update_callback = cb;
}

void
game_object_set_update_callback_script_func(game_object_t *obj,
                                            char *callback)
{
    update_callback_t *cb = malloc(sizeof(*cb));
    cb->type = SCRIPT_FUNC;
    cb->cb.script_func = strdup(callback);
    obj->update_callback = cb;
}

void
game_object_set_render_callback_c_func(game_object_t *obj,
                                       const game_object_render_fn callback)
{
    render_callback_t *cb = malloc(sizeof(*cb));
    cb->type = C_FUNC;
    cb->cb.c_func = callback;
    obj->render_callback = cb;
}

void
game_object_set_render_callback_script_func(game_object_t *obj,
                                            char * callback)
{
    render_callback_t *cb = malloc(sizeof(*cb));
    cb->type = SCRIPT_FUNC;
    cb->cb.script_func = strdup(callback);
    obj->render_callback = cb;
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
        return (obj->recv_callback)(mes);

    return 0;
}

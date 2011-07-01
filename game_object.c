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
    obj->update_callbacks = NULL;
    obj->update_callbacks_len = 0;
    obj->update_callbacks_cap = 0;
    obj->render_callbacks = NULL;
    obj->render_callbacks_len = 0;
    obj->render_callbacks_cap = 0;
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
game_object_destory(engine_t *eng, game_object_t *go)
{
    // TODO: ensure object is removed from all states
    game_state_remove_object(eng->state, go);

    game_object_clear_update_callbacks(go);
    game_object_clear_render_callbacks(go);
    
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

static void
stash_update_cb(game_object_t *obj,
                update_callback_t *cb)
{
    obj->update_callbacks =
        memory_grow_to_size(obj->update_callbacks,
                            sizeof(*obj->update_callbacks),
                            &obj->update_callbacks_cap,
                            obj->update_callbacks_len + 1);
    obj->update_callbacks[obj->update_callbacks_len] = cb;
    obj->update_callbacks_len++;
}

void
game_object_append_update_callback_c_func(game_object_t *obj,
                                          const game_object_update_fn callback)
{
    update_callback_t *cb = malloc(sizeof(*cb));
    cb->type = C_FUNC;
    cb->cb.c_func = callback;
    stash_update_cb(obj, cb);
}

update_callback_t *
game_object_remove_update_callback_c_func(game_object_t *obj,
                                          const game_object_update_fn callback)
{
    int i;
    for(i=0; i<obj->update_callbacks_len; i++)
        if(obj->update_callbacks[i] &&
           obj->update_callbacks[i]->cb.c_func == callback)
        {
            update_callback_t *cb = obj->update_callbacks[i];
            obj->update_callbacks[i] = NULL;
            /* TODO: attempt rebuild of cache? */
            return cb;
        }

    return NULL;
}

void
game_object_append_update_callback_script_func(game_object_t *obj,
                                               char *callback)
{
    update_callback_t *cb = malloc(sizeof(*cb));
    cb->type = SCRIPT_FUNC;
    cb->cb.script_func = strdup(callback);
    stash_update_cb(obj, cb);
}

update_callback_t *
game_object_remove_update_callback_script_func(game_object_t *obj,
                                               char *callback)
{
    int i;
    for(i=0; i<obj->update_callbacks_len; i++)
        if( obj->update_callbacks[i] && 
            strcmp(obj->update_callbacks[i]->cb.script_func, callback) == 0 )
        {
            update_callback_t *cb = obj->update_callbacks[i];
            obj->update_callbacks[i] = NULL;
            /* TODO: attempt rebuild of cache? */
            return cb;
        }

    return NULL;
}

static void
free_updates(update_callback_t *cb)
{
    switch(cb->type)
    {
    case C_FUNC:
        break;
    case SCRIPT_FUNC:
        free(cb->cb.script_func);
        break;
    default:
        assert(FALSE);
    }

    free(cb);
}

void
game_object_clear_update_callbacks(game_object_t *obj)
{
    int i;
    for(i=0; i<obj->update_callbacks_len; i++)
        if(obj->update_callbacks[i])
            free_updates(obj->update_callbacks[i]);
}

static void
stash_render_cb(game_object_t *obj,
                render_callback_t *cb)
{
    obj->render_callbacks =
        memory_grow_to_size(obj->render_callbacks,
                            sizeof(*obj->render_callbacks),
                            &obj->render_callbacks_cap,
                            obj->render_callbacks_len + 1);
    obj->render_callbacks[obj->render_callbacks_len] = cb;
    obj->render_callbacks_len++;
}

void
game_object_append_render_callback_c_func(game_object_t *obj,
                                          const game_object_render_fn callback)
{
    render_callback_t *cb = malloc(sizeof(*cb));
    cb->type = C_FUNC;
    cb->cb.c_func = callback;
    stash_render_cb(obj, cb);
}

render_callback_t *
game_object_remove_render_callback_c_func(game_object_t *obj,
                                          const game_object_render_fn callback)
{
    int i;
    for(i=0; i<obj->render_callbacks_len; i++)
        if(obj->render_callbacks[i] &&
           obj->render_callbacks[i]->cb.c_func == callback)
        {
            render_callback_t *cb = obj->render_callbacks[i];
            obj->render_callbacks[i] = NULL;
            /* TODO: attempt rebuild of cache? */
            return cb;
        }

    return NULL;
}

void
game_object_append_render_callback_script_func(game_object_t *obj,
                                               char * callback)
{
    render_callback_t *cb = malloc(sizeof(*cb));
    cb->type = SCRIPT_FUNC;
    cb->cb.script_func = strdup(callback);
    stash_render_cb(obj, cb);
}

render_callback_t *
game_object_remove_render_callback_script_func(game_object_t *obj,
                                               char * callback)
{
    int i;
    for(i=0; i<obj->render_callbacks_len; i++)
        if( obj->render_callbacks[i] && 
            strcmp(obj->render_callbacks[i]->cb.script_func, callback) == 0 )
        {
            render_callback_t *cb = obj->render_callbacks[i];
            obj->render_callbacks[i] = NULL;
            /* TODO: attempt rebuild of cache? */
            return cb;
        }

    return NULL;
}

static void
free_renders(render_callback_t *cb)
{
    switch(cb->type)
    {
    case C_FUNC:
        break;
    case SCRIPT_FUNC:
        free(cb->cb.script_func);
        break;
    default:
        assert(FALSE);
    }

    free(cb);
}

void
game_object_clear_render_callbacks(game_object_t *obj)
{
    int i;
    for(i=0; i<obj->render_callbacks_len; i++)
        if(obj->render_callbacks[i])
            free_renders(obj->render_callbacks[i]);
}

void
game_object_append_message(game_object_t *obj,
                           game_object_t *sender,
                           message_callback_func callback)
{
    message_t mes = message_construct(sender, obj, callback);
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

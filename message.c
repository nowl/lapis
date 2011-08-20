#include "lapis.h"

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

message_t *
message_create(game_object_t *sender,
               game_object_t *receiver, 
               char *type,
               void *data,
               char own_data)
{
    message_t *mes = malloc(sizeof(*mes));
    mes->sender = sender;
    mes->receiver = receiver;
    mes->type = lapis_hash(type);
    mes->data = data;
    mes->own_data = own_data;
    return mes;
}

message_t *
message_copy(message_t *mes)
{
    message_t *m = malloc(sizeof(*m));
    memcpy(m, mes, sizeof(*m));
    m->own_data = 0;
    return m;
}

void
message_create_and_send(char *sender,
                        char *receiver, 
                        char *type,
                        void *data,
                        char own_data,
                        int delivery_type)
{
    game_object_t *sobj = sender ? game_object_get_by_name(sender) : NULL;
    game_object_t *robj = receiver ? game_object_get_by_name(receiver) : NULL;
    message_t *mes = message_create(sobj, robj, type, data, own_data);
    message_deliver(mes, delivery_type);
    if(delivery_type == SYNC)
        free(mes);
}


/*
message_t
message_construct(game_object_t *sender,
                  game_object_t *receiver,
                  char *type,
                  void *data,
                  char own_data)
{
    message_t mes;
    mes.sender = sender;
    mes.receiver = receiver;
    mes.type = lapis_hash(type);
    mes.data = data;
    mes.own_data = own_data;
    return mes;
}
*/

void
message_destroy(message_t *message)
{
    if(message->own_data)
        free(message->data);
    free(message);
}

void
message_deliver(message_t *mes, int type)
{
    game_state_t * state = lapis_get_engine()->state;
    switch(type)
    {
    case ASYNC:
        game_state_deliver_message_async(state, mes);
        break;
    case SYNC:
        game_state_deliver_message_sync(state, mes);
        break;
    }
}

#include "lapis.h"

message_t *
message_create(game_object_t *sender, game_object_t *receiver, message_callback_func callback_func)
{
    message_t *mes = malloc(sizeof(*mes));
    mes->sender = sender;
    mes->receiver = receiver;
    mes->callback_func = callback_func;
    return mes;
}

message_t
message_construct(game_object_t *sender, game_object_t *receiver, message_callback_func callback_func)
{
    message_t mes;
    mes.sender = sender;
    mes.receiver = receiver;
    mes.callback_func = callback_func;
    return mes;
}

void
message_destroy(message_t *message)
{
    free(message);
}

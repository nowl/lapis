#ifndef __MESSAGE_H__
#define __MESSAGE_H__

#define MAX_PAYLOAD_SIZE 32

struct message
{
    int type;
    struct entity* source;
    char payload[MAX_PAYLOAD_SIZE];

    struct message *next, *prev;
};

enum message_del_type
{
    MDT_SYNC,
    MDT_ASYNC
};

void message_send(int type, struct entity *source, void *payload, size_t payload_size, enum message_del_type del_type);

void message_cleanup_mempools();

#endif  /* __MESSAGE_H__ */

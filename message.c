#include "lapis.h"

static struct mempool *messages_mp = NULL;

static struct message *messages = NULL;

static int
process_message(struct message *m)
{
    struct component * c = component_responder_lookup(m->type);
}

void message_send(int type,
                  struct entity *source, 
                  void *payload, 
                  size_t payload_size,
                  enum message_del_type del_type)
{
    assert(payload_size <= MAX_PAYLOAD_SIZE);

    if( !messages_mp)
        messages_mp = mp_create(sizeof(struct message), MEMPOOL_DEFAULT_SIZE);

    struct message *m = mp_alloc(messages_mp);
    m->type = type;
    m->source = source;
    memcpy(m->payload, payload, payload_size);

    if(del_type == MDT_ASYNC)
        process_message(m);
    else
        LL_APPEND(messages, m);
}

static void message_cleanup(struct message *m)
{
    mp_free(messages_mp, m);
}

void message_cleanup_mempools()
{
    free(messages_mp);
}


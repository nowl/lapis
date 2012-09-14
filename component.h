#ifndef __COMPONENT_H__
#define __COMPONENT_H__

#include <utlist.h>

typedef int (*component_responder_cb)(void *, void *);

struct component
{
    char *name;
    component_responder_cb cb;
};

struct component_list
{
    struct component component;
    struct component_list *next, *prev;
};

struct component *create_component(char *name);
void destroy_component(struct component *c);

void component_add_responder(struct component *comp, int type);
struct component_list *component_responder_lookup(int type);

void component_cleanup_mempools();

#endif  /* __COMPONENT_H__ */

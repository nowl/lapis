#include "lapis.h"

list_t *list_create(void *data)
{
    list_t *n = malloc(sizeof(*n));
    n->data = data;
    n->next = n->prev = NULL;
    return n;
}

void list_destroy(list_t *n)
{
    free(n);
}

list_t *list_append(list_t* list, list_t *entry)
{
    /* go to end */
    if(list)
    {
        list_t *p;
        for(; list; p=list, list=list->next) {}
        p->next = entry;
        entry->prev = p;
    }
   
    return entry;
}

list_t *list_remove(list_t* entry)
{
    list_t *p = entry->prev;
    if(p) p->next = entry->next;
    list_t *n = entry->next;
    if(n) n->prev = entry->prev;
   
    if(n)
        return n;
    else
        return p;
}

list_t *list_first(list_t* list)
{
    if(list)
    {
        list_t *p;
        for(; list; p=list, list=list->prev) {}
        return p;
    }
    else
        return NULL;
}    

#include "lapis.h"

void cleanup_mempools()
{
    //entity_cleanup_mempools();
    component_cleanup_mempools();
    message_cleanup_mempools();
}

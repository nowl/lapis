#include "lapis.h"

int main(int argc, char *argv[])
{
    lapis_init();

    engine_t *eng = engine_create();

    engine_quit(eng);

    mainloop(eng);

    engine_destroy(eng);
    
    lapis_deinit();

    return 0;
}

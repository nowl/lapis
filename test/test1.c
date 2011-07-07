#include "lapis.h"

int main(int argc, char *argv[])
{
    lapis_init();

    engine_t *eng = engine_create();

    engine_quit(eng);

    /*
    image_loader_load("test", "handle-8.png", 
                      0, 0, 16, 16);

    SDL_Surface* s = image_loader_get("test");
    */

    mainloop(eng);

    engine_destroy(eng);
    
    lapis_deinit();

    return 0;
}

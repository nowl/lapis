#include "lapis.h"

int main(int argc, char *argv[])
{
    lapis_init();

    engine_t *engine = lapis_get_engine();

    game_state_t *state = game_state_create(0);

    engine_switch_state(engine, state);

    /* initialize video */
    
    lsdl_set_video_mode(engine->sdl_driver,
                        1024, 768, 0);

    /* create object */

    game_object_t * obj = game_object_create(0, NULL);
    game_state_append_object(state, obj);
    game_state_append_bcast_recvr(state, obj, message_type_hash("sdl-event"));
        
    //engine_quit(engine);

    image_loader_load("test", "screenshot1.jpg", 
                      0, 0, 16, 16);

    SDL_Surface* s = image_loader_get("test");
    
    image_render_set_create("test_set");
    image_render_set_add("test_set", "test", 10);
    image_render_set_get_image("test_set", 10);

    lapis_mainloop();

    game_state_destroy(state);
        
    lapis_deinit();

    return 0;
}

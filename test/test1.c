#include "lapis.h"

int message_handler_2(message_t mes)
{
    return 0;
}                    

int message_handler(message_t mes)
{
    if(mes.type == message_type_hash("sdl-event"))
    {
        SDL_Event event = *(SDL_Event *)mes.data;
        LOG("sdl-event broadcast (type: %d)\n", event.type);
        if(event.type == SDL_KEYDOWN)
        {
            if(event.key.keysym.sym == SDLK_ESCAPE)
            {
                engine_quit(lapis_get_engine());
                return 1;
            }
        }
    }

    return 0;
}                    

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

    game_object_t * obj1 = game_object_create(0, NULL);
    game_state_append_object(state, obj1);
    game_object_set_recv_callback_c_func(obj1, message_handler_2);
    game_state_append_bcast_recvr(state, obj1, message_type_hash("sdl-event"));
    
    game_object_t * obj2 = game_object_create(1, NULL);
    game_state_append_object(state, obj2);
    game_object_set_recv_callback_c_func(obj2, message_handler);
    game_state_append_bcast_recvr(state, obj2, message_type_hash("sdl-event"));
        
    //engine_quit(engine);

    image_loader_load("test", "screenshot1.jpg", 
                      0, 0, 16, 16);

    SDL_Surface* s = image_loader_get("test");
    
    image_render_set_create("test_set");
    image_render_set_add("test_set", "test", 10);
    image_render_set_get_image("test_set", 10);

    lapis_mainloop();

    /* this will be cleaned up by the OS */
    //game_state_destroy(state);
    //lapis_deinit();

    return 0;
}

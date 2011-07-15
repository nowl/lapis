#include "lapis.h"

typedef struct
{
    int x, y;
} player_object_t;

int message_handler_2(game_object_t *obj, message_t mes)
{
    if(mes.type == message_type_hash("sdl-event"))
    {
        SDL_Event event = *(SDL_Event *)mes.data;
        if(event.type == SDL_KEYDOWN)
        {
            switch(event.key.keysym.sym)
            {
            case SDLK_a:
                LOG("pressed a\n");
                return 1;
            case SDLK_UP:
            {
                player_object_t *data = obj->data;
                data->y -=5;
                return 1;
            }
            case SDLK_DOWN:
            {
                player_object_t *data = obj->data;
                data->y +=5;
                return 1;
            }
            case SDLK_LEFT:
            {
                player_object_t *data = obj->data;
                data->x -=5;
                return 1;
            }
            case SDLK_RIGHT:
            {
                player_object_t *data = obj->data;
                data->x +=5;
                return 1;
            }
            default:
                break;
            }
        }
    }

    return 0;
}                    

int message_handler(game_object_t *obj, message_t mes)
{
    if(mes.type == message_type_hash("sdl-event"))
    {
        SDL_Event event = *(SDL_Event *)mes.data;
        if(event.type == SDL_KEYDOWN)
        {
            switch(event.key.keysym.sym)
            {
            case SDLK_ESCAPE:
                engine_quit(lapis_get_engine());
                return 1;
            default:
                break;
            }
        }
    }

    return 0;
}                    

void render_1(engine_t *engine, game_object_t *obj, float interpolation)
{
    player_object_t *data = obj->data;
    lsdl_draw_image(engine, image_loader_get("test"), data->x, data->y);
    //lsdl_fill_rect(engine, data->x, data->y, 64, 64, 0, 0, 200);
}

int main(int argc, char *argv[])
{
    lapis_init();

    engine_t *engine = lapis_get_engine();
    set_ticks_per_second(60);

    game_state_t *state = game_state_create(0);

    engine_switch_state(engine, state);

    /* initialize video */
    
    lsdl_set_video_mode(engine->sdl_driver,
                        800, 600, 0);

    SDL_EnableKeyRepeat(500, 50);

    /* load graphics */
    image_loader_load("test", "screenshot1.jpg", 100, 100, 32, 32);

    /* create object */

    game_object_t * obj1 = game_object_create(0, NULL);
    game_state_append_object(state, obj1);
    game_object_set_recv_callback_c_func(obj1, message_handler);
    game_state_append_bcast_recvr(state, obj1, message_type_hash("sdl-event"));
    
    player_object_t player_data;
    player_data.x = 400;
    player_data.y = 400;

    game_object_t * obj2 = game_object_create(1, &player_data);
    game_state_append_object(state, obj2);
    game_object_set_recv_callback_c_func(obj2, message_handler_2);
    game_state_append_bcast_recvr(state, obj2, message_type_hash("sdl-event"));
    game_object_set_render_callback_c_func(obj2, render_1);
        
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

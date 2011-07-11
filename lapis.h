#ifndef __LAPIS_H__
#define __LAPIS_H__

#include <memory.h>
#include <assert.h>

#include <SDL.h>
#include <SDL_image.h>
#include <SDL_ttf.h>
#include <SDL_mixer.h>

#ifdef NDEBUG
# define LOG
#else
# define LOG(...) { fprintf(stdout, "[L] %s:%d -> ", __FILE__, __LINE__); fprintf(stdout, __VA_ARGS__); }
#endif

#define WARN(...) { fprintf(stdout, "[W] %s:%d -> ", __FILE__, __LINE__); fprintf(stdout, __VA_ARGS__); }
#define ERROR(...) { fprintf(stdout, "[E] %s:%d -> ", __FILE__, __LINE__); fprintf(stdout, __VA_ARGS__); }

#ifndef FALSE
# define FALSE 0
# define TRUE (!FALSE)
#endif

typedef struct engine engine_t;
typedef struct sdl_graphics_context sdl_graphics_context_t;
typedef struct bcast_recvr bcast_recvr_t;
typedef struct game_state game_state_t;
typedef struct game_object game_object_t;
typedef struct update_callback update_callback_t;
typedef struct render_callback render_callback_t;
typedef struct message message_t;

//typedef void (*message_callback_func)(game_object_t *sender, game_object_t *receiver, void *data);
typedef void (*game_object_update_fn)(engine_t *engine, game_object_t *obj, unsigned int ticks);
typedef void (*game_object_render_fn)(engine_t *engine, game_object_t *obj, float interpolation);

struct message
{
    game_object_t *sender, *receiver;
    unsigned long type;
    void *data;
};

struct bcast_recvr
{
    game_object_t *obj;         /* the object listening to this broadcast message */
    unsigned long hash;         /* the hashed broadcast message type */
};

struct game_state
{
    int id;

    game_object_t **objects;
    size_t objects_len;
    size_t objects_cap;

    //game_state_update_fn update_fn;
    //game_state_event_handle_fn event_handle_fn;
    //game_state_render_fn render_fn;
    
    bcast_recvr_t *bcast_recvrs;
    size_t bcast_recvrs_len;
    size_t bcast_recvrs_cap;
};

enum callback_types
{
    C_FUNC,
    SCRIPT_FUNC
};

struct update_callback {
    enum callback_types type;

    union {
        game_object_update_fn c_func;
        char *script_func;
    } cb;
};

struct render_callback {
    enum callback_types type;

    union {
        game_object_render_fn c_func;
        char *script_func;
    } cb;
};

struct game_object
{
    unsigned long id;
    unsigned int type;
    void *data;
	SDL_Surface *image;
	int screenx;
	int screeny;

    /* TODO: possibly make these stack allocated like messages? */
    update_callback_t **update_callbacks;
    size_t update_callbacks_len;
    size_t update_callbacks_cap;
    render_callback_t **render_callbacks;
    size_t render_callbacks_len;
    size_t render_callbacks_cap;
    message_t *messages;
    size_t messages_len;
    size_t messages_cap;
};

struct engine
{
    sdl_graphics_context_t *sdl_driver;
    // font *font_driver;

#define MAX_GAME_STATES 100

    game_state_t* state;
    unsigned char is_running;
    unsigned int tick;

	int fps;

    //void *script_controller;
    //int (*script_update_call)(char *, game_manager *, game_object *, int);
    //int (*script_render_call)(char *, game_manager *, game_object *, float);
};

struct sdl_graphics_context
{
    SDL_Surface* screen;

    SDL_Rect *dirty_rects;
    int dirty_rects_i;
    SDL_Rect *erase_rects;
    int erase_rects_i;

    /* flag that determines whether to use dirty rects or not */
	int full_screen_update;
};

/* sdl_graphics_context */

unsigned int lsdl_get_tick();
void lsdl_set_video_mode(sdl_graphics_context_t* gc,
                         unsigned int screen_width,
                         unsigned int screen_height,
                         unsigned char fullscreen);
void lsdl_fill_rect(engine_t *manager, int x, int y, 
                    int w, int h, 
                    int red, int green, int blue);
void lsdl_dirty_rect(engine_t *manager, int x, int y, 
                     int w, int h);
void lsdl_draw_image(engine_t *manager, SDL_Surface *surf, 
                     int x, int y);
void lsdl_flip(engine_t * manager);

/* lapis */

int       lapis_init();
void      lapis_deinit();
engine_t *lapis_get_engine();
void      lapis_mainloop();

/* engine */

engine_t *engine_create();
void      engine_destroy(engine_t *gm);
void      engine_switch_state(engine_t *gm, game_state_t* state);
void      engine_handle_events(engine_t *gm);
void      engine_update(engine_t *gm, unsigned int ticks);
void      engine_render(engine_t *gm, float interpolation);
void      engine_start(engine_t *gm);
void      engine_quit(engine_t *gm);


/* utils */

void *memory_grow_to_size(void *mem_p,
                          size_t per_block,
                          size_t *mem_cap_ip,
                          size_t mem_size_i);

/* game_state */

game_state_t * game_state_create(int id);
void           game_state_destroy(game_state_t *state);
void           game_state_update(engine_t *, unsigned int ticks);
void           game_state_render(engine_t *, float interpolation);
void           game_state_append_object(game_state_t *, game_object_t *);
game_object_t* game_state_remove_object(game_state_t *, game_object_t *);
void           game_state_append_bcast_recvr(game_state_t *state, game_object_t *obj, unsigned long hash);
void           game_state_deliver_message_sync(game_state_t *state, message_t message);
void           game_state_deliver_message_async(game_state_t *state, message_t message);

/* sdl_font */

SDL_Surface * sdl_font_get_surf(char *font_name, int pt_size, char *text, int r, int g, int b);
int           sdl_font_get_size(char *font_name, int pt_size, char *text, int *w, int *h);
void          lsdl_free_font_surfaces();
void          sdl_font_cleanup();

/* sdl_sound */

int  sdl_sound_load(char *file, char *alias);
void sdl_play_sound(char *file);
void sound_loader_cleanup();

/* game_object */

game_object_t * game_object_create(unsigned int type, void *data);
void            game_object_destroy(engine_t *eng, game_object_t *go);
game_object_t * game_object_get(int id);
game_object_t * game_object_remove(game_object_t *obj);
game_object_t * game_object_remove_by_id(int id);

void game_object_append_update_callback_c_func(game_object_t *obj,
                                               const game_object_update_fn callback);
update_callback_t * game_object_remove_update_callback_c_func(game_object_t *obj,
                                                              const game_object_update_fn callback);
void game_object_append_update_callback_script_func(game_object_t *obj,
                                                    char *callback);

update_callback_t *
game_object_remove_update_callback_script_func(game_object_t *obj,
                                               char *callback);
void
game_object_clear_update_callbacks(game_object_t *obj);

update_callback_t *
game_object_update_callback_next(game_object_t *obj,
                                 update_callback_t *cur);

void
game_object_append_render_callback_c_func(game_object_t *obj,
                                          const game_object_render_fn callback);

render_callback_t *
game_object_remove_render_callback_c_func(game_object_t *obj,
                                   const game_object_render_fn callback);

void
game_object_append_render_callback_script_func(game_object_t *obj,
                                   char * callback);

render_callback_t *
game_object_remove_render_callback_script_func(game_object_t *obj,
                                   char * callback);

void
game_object_clear_render_callbacks(game_object_t *obj);

render_callback_t *
game_object_render_callback_next(game_object_t *obj,
                                 render_callback_t *cur);

void
game_object_append_message(game_object_t *obj,
                           game_object_t *sender,
                           char *type,
                           void *data);

void
game_object_clear_messages(game_object_t *obj);

message_t *
game_object_message_next(game_object_t *obj,
                         message_t *cur);

/* message */

/*
message_t * message_create(game_object_t            *sender,
                           game_object_t            *receiver,
                           message_callback_func     callback_func,
                           char                     *type,
                           void                     *data);
void        message_destroy(message_t               *message);
*/
message_t   message_construct(game_object_t         *sender,
                              game_object_t         *receiver,
                              char                  *type,
                              void                  *data);

unsigned long message_type_hash(char *type);

/* message deliver type */
enum
{
    SYNC,
    ASYNC
};

void message_deliver(message_t mes, int type);

/* mainloop */

void mainloop(engine_t* eng);
void set_ticks_per_second(unsigned int num);
void set_max_frame_skip(unsigned int num);

/* image_loader */

int
image_loader_load(char *alias,
                  char *filename,
                  int x,
                  int y,
                  int width,
                  int height);

SDL_Surface *image_loader_get(char *alias);
void         image_loader_cleanup();

/* image_render_set */

void          image_render_set_create(char *name);
void          image_render_set_add(char *name, char *image_name, int num_ticks);
void          image_render_set_cleanup();
SDL_Surface * image_render_set_get_image(char *name, int cur_tick);

/* collide */

int collide_point_in_rect(float x, float y, SDL_Rect *rect);
int collide_lines_intersect(float x1, float y1, float x2,
                            float y2, float x3, float y3,
                            float x4, float y4);
int collide_point_in_polygon(float x, float y,
                             float *polygon, int num_points);
int collide_rect_intersect(SDL_Rect *bb1, SDL_Rect *bb2);
int collide_polygon_intersect(float *poly1, int poly1_num_points,
                              float *poly2, int poly2_num_points);

/* astar */

struct astar_pos_t {
	unsigned int x, y;
};

typedef struct astar_pos_t* astar_pos_vector_t;

typedef int (*move_cost_fn)(unsigned int x, unsigned int y);

void astar_init(int width,
                int height,
                move_cost_fn fn);
void astar_destroy();

/* It is the user's responsibility to free the pointer returned by
 * this function when finished with it. */
void astar_best_path(struct astar_pos_t begin,
					 struct astar_pos_t end);
astar_pos_vector_t astar_retrieve_path();
int astar_retrieve_path_length();

#endif  /* __LAPIS_H__ */

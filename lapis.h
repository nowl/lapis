#ifndef __LAPIS_H__
#define __LAPIS_H__

#include <memory.h>
#include <assert.h>
#include <time.h>
#include <limits.h>

#include <SDL.h>
#include <GL/gl.h>
#include <GL/glu.h>
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
typedef struct aatree_node aatree_node_t;
typedef struct list list_t;
typedef struct ref ref_t;
typedef struct lapis_lua lapis_lua_t;

//typedef void (*message_callback_func)(game_object_t *sender, game_object_t *receiver, void *data);
typedef void (*game_object_update_fn)(engine_t *engine, game_object_t *obj, unsigned int ticks);
typedef void (*game_object_render_fn)(engine_t *engine, game_object_t *obj, float interpolation);
typedef int  (*recv_callback_fn)(game_object_t *obj, message_t *mes);

struct ref
{
    int count;
    void *data;
};

struct list
{
    list_t *next, *prev;
    void *data;
};

struct aatree_node
{
    aatree_node_t  *left, *right, *parent;
    int             level;
    unsigned long   hash;
    void           *data;
    int             owns_data;
};

struct message
{
    game_object_t *sender, *receiver;
    unsigned long type;
    ref_t *data;
};

struct bcast_recvr
{
    game_object_t *obj;         /* the object listening to this broadcast message */
    unsigned long hash;         /* the hashed broadcast message type */
};

struct game_state
{
    int            id;
    aatree_node_t *objects;
    list_t        *bcast_recvrs;
    int            num_render_levels;
};

enum callback_types
{
    NONE,
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
    unsigned long name;
    unsigned int type;
    
    int render_level;
    void *data;
	SDL_Surface *image;
	int screenx;
	int screeny;

    update_callback_t   update_callback;
    render_callback_t   render_callback;
    recv_callback_fn    recv_callback; /* message receive callback */
    
    message_t **messages;
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
    int flags_for_setvideomode;
};

/* sdl_graphics_context */

unsigned int lsdl_get_tick();
void lsdl_set_video_mode(sdl_graphics_context_t* gc,
                         unsigned int screen_width,
                         unsigned int screen_height,
                         unsigned char fullscreen,
                         unsigned char resizable);
void lsdl_resize_internal(int w, int h);
void lsdl_fill_rect(engine_t *manager, float x, float y, 
                    float w, float h, 
                    float red, float green, float blue);
void lsdl_draw_image(engine_t *manager, GLuint texture,
                     float x, float y, float w, float h,
                     float r, float g, float b);
void lsdl_draw_text(engine_t *engine,
                    char *font_name, int pt_size,
                    char *text, int r, int g, int b,
                    int x, int y);
void lsdl_flip(engine_t * manager);
void lsdl_prepare_render();

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
void           game_state_append_bcast_recvr(game_state_t *state, game_object_t *obj, char *name);
void           game_state_deliver_message_sync(game_state_t *state, message_t *message);
void           game_state_deliver_message_async(game_state_t *state, message_t *message);

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

game_object_t     *game_object_create(char *name, void *data);
void               game_object_destroy(engine_t *eng, game_object_t *go);
game_object_t     *game_object_get_by_name(char *name);
game_object_t     *game_object_remove(game_object_t *go);
game_object_t     *game_object_remove_by_name(char *name);
void               game_object_set_recv_callback_c_func(game_object_t *obj,
                                                        recv_callback_fn callback);
void               game_object_set_update_callback_c_func(game_object_t *obj,
                                                          const game_object_update_fn callback);
void               game_object_set_update_callback_script_func(game_object_t *obj,
                                                               char *callback);
void               game_object_set_render_callback_c_func(game_object_t *obj,
                                                          const game_object_render_fn callback);
void               game_object_set_render_callback_script_func(game_object_t *obj,
                                                               char * callback);
void               game_object_append_message(game_object_t *obj,
                                              message_t *mes);
void               game_object_clear_messages(game_object_t *obj);
int                game_object_recv_mes(game_object_t *obj, message_t *mes);
void               game_object_process_messages(game_object_t *obj);

/* message */

message_t * message_create(game_object_t            *sender,
                           game_object_t            *receiver,
                           char                     *type,
                           ref_t                    *data);

message_t * message_copy(message_t *mes);

void        message_create_and_send(char  *sender,
                                    char  *receiver, 
                                    char  *type,
                                    ref_t *data,
                                    int    delivery_type);

void        message_destroy(message_t               *message);

/*
message_t   message_construct(game_object_t         *sender,
                              game_object_t         *receiver,
                              char                  *type,
                              void                  *data);
*/

unsigned long lapis_hash(char *type);

/* message deliver type */
enum
{
    SYNC,
    ASYNC
};

void message_deliver(message_t *mes, int type);

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

GLuint     image_loader_get(char *alias);
void       image_loader_cleanup();
GLuint     opengl_texture_from_surface(SDL_Surface *image); /* helper function */

/* image_render_set */

void          image_render_set_create(char *name);
void          image_render_set_add(char *name, char *image_name, int num_ticks);
void          image_render_set_cleanup();
GLuint        image_render_set_get_image(char *name, int cur_tick);

/* collide */

int collide_point_in_rect(float x, float y, SDL_Rect *rect);
int collide_lines_intersect(float x1, float y1, float x2,
                            float y2, float x3, float y3,
                            float x4, float y4);
int collide_line_rect(float x1, float y1, float x2, float y2,
                      float llx, float lly,
                      float lrx, float lry,
                      float urx, float ury,
                      float ulx, float uly);
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
					 struct astar_pos_t end,
                     int bailout_tries);
astar_pos_vector_t astar_retrieve_path();
int astar_retrieve_path_length();

/* random */

unsigned long random_ul();
void          random_init();
float         random_float();
int           random_int_min_max(int min, int max);

/* aatree */
aatree_node_t *aatree_first(aatree_node_t *root);
aatree_node_t *aatree_next(aatree_node_t *n);
aatree_node_t *aatree_find(aatree_node_t *root, unsigned long hash);
aatree_node_t *aatree_insert(aatree_node_t *T, aatree_node_t *n);
aatree_node_t *aatree_delete(aatree_node_t *T, aatree_node_t *n);
aatree_node_t *aatree_create(unsigned long hash, void *data, char owns_data);

/* list */
list_t *list_create(void *data);
void    list_destroy(list_t *n);
list_t *list_append(list_t* list, list_t *entry);
list_t *list_remove(list_t* entry);
list_t *list_first(list_t* list);

/* ref */
ref_t *ref_create(void * data);
void   ref_inc(ref_t *ref);
void   ref_dec(ref_t *ref);

/* lua scripting */
lapis_lua_t *lua_scripting_init();
void lua_scripting_destroy(lapis_lua_t *ll);
int lua_scripting_run_file(lapis_lua_t *ll, char *filename);

/* los */
typedef float (*opaque_f)(int x, int y);
typedef void (*set_los_f)(int x, int y, float visibility);

void los_init();
void los_run(int origin_x, int origin_y, opaque_f opfun, set_los_f visibility);

#endif  /* __LAPIS_H__ */

#include "lapis.h"

/* quick cache will contain the previously loaded "raw" image */
static char *quick_cache_filename = NULL;
static SDL_Surface *quick_cache = NULL;

static struct {
    /* reference to the texture object */
    GLuint texture;

    /* a hashed version of the string representing the alias of this image */
    unsigned long alias_hash;
} *cache = NULL;
static size_t cache_len = 0;
static size_t cache_cap = 0;


static GLuint
opengl_texture_from_surface(SDL_Surface *image)
{
    GLuint texture;
    GLenum texture_format;
    GLint  nOfColors;
    
    // Check that the image's width is a power of 2
    if ( (image->w & (image->w - 1)) != 0 )
        WARN("warning: texture: width is not a power of 2\n");
    // Also check if the height is a power of 2
    if ( (image->h & (image->h - 1)) != 0 )
        WARN("warning: texture: height is not a power of 2\n");
    // get the number of channels in the SDL image
    nOfColors = image->format->BytesPerPixel;
    if (nOfColors == 4)     // contains an alpha channel
    {
        if (image->format->Rmask == 0x000000ff)
            texture_format = GL_RGBA;
        else
            texture_format = GL_BGRA;
    } else if (nOfColors == 3)     // no alpha channel
    {
        if (image->format->Rmask == 0x000000ff)
            texture_format = GL_RGB;
        else
            texture_format = GL_BGR;
    } else {
        WARN("warning: the image is not truecolor..  this will probably break\n");
        return -1;
    }

    // Have OpenGL generate a texture object handle for us
    glGenTextures( 1, &texture );

    // Bind the texture object
    glBindTexture(GL_TEXTURE_2D, texture);

    // Set the texture's stretching properties
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR );
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR );
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S,     GL_CLAMP);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T,     GL_CLAMP);

    // Edit the texture object's image data using the information SDL_Image gives us
    glTexImage2D( GL_TEXTURE_2D, 0, nOfColors, image->w, image->h, 0,
                  texture_format, GL_UNSIGNED_BYTE, image->pixels );

    return texture;
}

int
image_loader_load(char *alias,
                  char *filename,
                  int x,
                  int y,
                  int width,
                  int height)
{
    SDL_Surface *raw;

    /* check quick cache */
    if(!quick_cache_filename || strcmp(filename, quick_cache_filename) != 0) {
        raw = IMG_Load(filename);
        if(!raw)
        {
            LOG("IMG_Load: %s\n", IMG_GetError());
            return -1;
        }

        /* free if necessary */
        if(quick_cache_filename)
            free(quick_cache_filename);
        if(quick_cache)
            SDL_FreeSurface(quick_cache);

        quick_cache = raw;
        quick_cache_filename = strdup(filename);
        LOG("quick caching \"%s\"\n", filename);
    } else {
        raw = quick_cache;
    }

    SDL_Surface *image = SDL_CreateRGBSurface(0, width, height,
                                              raw->format->BitsPerPixel,
                                              raw->format->Rmask,
                                              raw->format->Gmask,
                                              raw->format->Bmask,
                                              raw->format->Amask);
    /* ignore alpha info */
    SDL_SetAlpha(image, 0, 0);
    /* XXX: set color key to 0 to mask out the transparent parts*/
    SDL_SetColorKey(image, SDL_SRCCOLORKEY, 0);

    if(!image)
    {
        SDL_FreeSurface(raw);
        free(quick_cache_filename);
        quick_cache_filename = NULL;
        quick_cache = NULL;
        LOG("SDL_CreateRGBSurface fail\n");
        return -1;
    }

    SDL_Rect source_rect = {x, y, width, height};

    int r = SDL_BlitSurface(raw, &source_rect, image, NULL);
    if(r != 0)
    {
        SDL_FreeSurface(image);
        SDL_FreeSurface(raw);
        free(quick_cache_filename);
        quick_cache_filename = NULL;
        quick_cache = NULL;
        LOG("SDL_BlitSurface fail: %s\n", SDL_GetError());
        return -1;
    }
    
    int index = cache_len++;
    cache = memory_grow_to_size(cache,
                                sizeof(*cache),
                                &cache_cap,
                                cache_len);

    GLuint texture = opengl_texture_from_surface(image);
    
    // free the SDL_Surface since it is no longer needed
    if (image)
        SDL_FreeSurface(image);
    
    cache[index].texture = texture;
    cache[index].alias_hash = lapis_hash(alias);

    LOG("cached cropped surface with alias \"%s\" -> hash (0x%lx) to texture id %d\n", alias, cache[index].alias_hash, texture);

    return 0;
}

GLuint
image_loader_get(char *alias)
{
    int i;
    unsigned long hash = lapis_hash(alias);
    for(i=0; i<cache_len; i++)
    {
        if(cache[i].alias_hash == hash)
            return cache[i].texture;
    }
    
    ERROR("couldn't find cached image for alias \"%s\"\n", alias);
    return 0;
}

void
image_loader_cleanup()
{
    int i;
    for(i=0; i<cache_len; i++)
        glDeleteTextures( 1, &cache[i].texture );

    free(cache);
    cache = NULL;
    cache_len = 0;
    cache_cap = 0;
}

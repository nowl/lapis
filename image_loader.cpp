#include <SDL_image.h>

#include "image_loader.hpp"

#include "log.hpp"

ImageLoader::ImageLoader()
    : _cachedImageFilename(""),
      _cachedImage(NULL)
{}

ImageLoader::~ImageLoader()
{
    if(_cachedImage)
        SDL_FreeSurface(_cachedImage);

    auto iter = _cache.begin();
    for(; iter != _cache.end(); ++iter)
    {
        GLuint *p = &iter->second;
        glDeleteTextures( 1, p );
    }
}

int ImageLoader::load(std::string alias,
                      std::string filename,
                      short x,
                      short y,
                      short width,
                      short height)
{
    SDL_Surface *raw;
  
    /* check cache */
    if(filename != _cachedImageFilename)
    {
        raw = IMG_Load(filename.c_str());
  
        if(!raw)
        {
            LOG("IMG_Load: %s\n", IMG_GetError());
            return -1;
        }

        /* free if necessary */
        if(_cachedImage)
            SDL_FreeSurface(_cachedImage);

        _cachedImage = raw;
        _cachedImageFilename = filename;
        LOG("caching image \"%s\"\n", filename.c_str());
    } else {
        raw = _cachedImage;
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
        _cachedImage = NULL;
        _cachedImageFilename = "";
        LOG("SDL_CreateRGBSurface fail\n");
        return -1;
    }

    SDL_Rect source_rect = {x, y, width, height};

    int r = SDL_BlitSurface(raw, &source_rect, image, NULL);
    if(r != 0)
    {
        SDL_FreeSurface(image);
        SDL_FreeSurface(raw);
        _cachedImage = NULL;
        _cachedImageFilename = "";
        LOG("SDL_BlitSurface fail: %s\n", SDL_GetError());
        return -1;
    }

    _cache[alias] = _createTextureFromSurface(image);
    
    // free the SDL_Surface since it is no longer needed
    if (image)
        SDL_FreeSurface(image);

    LOG("cached cropped surface with alias \"%s\" -> to texture id %d\n", alias.c_str(), _cache[alias]);

    return 0;
}

GLuint ImageLoader::_createTextureFromSurface(SDL_Surface *image)
{
    GLuint texture;
    GLenum texture_format;
    GLint  nOfColors;
    
    // Check that the image's width is a power of 2
    //if ( (image->w & (image->w - 1)) != 0 ) WARN("warning: texture: width is not a power of 2\n");
    // Also check if the height is a power of 2
    //if ( (image->h & (image->h - 1)) != 0 ) WARN("warning: texture: height is not a power of 2\n");
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
        WARN("warning: the image is not truecolor..  this will probably break (%d)\n", nOfColors);
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

GLuint
ImageLoader::get(std::string alias)
{
    if(_cache.find(alias) == _cache.end())
    {
        ERROR("couldn't find cached image for alias \"%s\"\n", alias.c_str());
        return 0;
    }
    
    return _cache[alias];
}

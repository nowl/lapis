#ifndef __IMAGE_LOADER_HPP__
#define __IMAGE_LOADER_HPP__

#include <string>
#include <unordered_map>

#include <SDL.h>
#include <GL/gl.h>

class ImageLoader {
public:
    ImageLoader();    
    ~ImageLoader();

    int load(std::string alias,
             std::string filename,
             short x,
             short y,
             short width,
             short height);

    GLuint get(std::string alias);

private:
    std::string _cachedImageFilename;
    SDL_Surface *_cachedImage;
    std::unordered_map<std::string, GLuint> _cache;

    GLuint _createTextureFromSurface(SDL_Surface *image);
};

#endif  // __IMAGE_LOADER_HPP__

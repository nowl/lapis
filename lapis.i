%module lapis
%{
#include "lapis.h"
%}
int lapis_init();

int image_loader_load(char *alias,
                      char *filename,
                      int x,
                      int y,
                      int width,
                      int height);


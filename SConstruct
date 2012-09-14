# -*- mode: python -*-

#vars = Variables()
#vars.Add(BoolVariable('SYNTAX', 'Set to 1 for a syntax check',0))

#env = Environment(variables=vars)
env = Environment()

t = env.SharedLibrary(target = 'lapis',
                      source = [#'engine.c',
                                #'utils.c',
                                #'lapis.c',
                                #'game_state.c',
                                #'sdl_graphics_context.c',
                                #'sdl_font.c',
                                #'game_object.c',
                                #'message.c',
                                #'mainloop.c',
                                #'image_loader.c',
                                #'image_render_set.c',
                                #'collide.c',
                                #'astar.c',
                                #'random.c',
                                #'aatree.c',
                                #'list.c',
                                #'ref.c',
                                #'lapis_wrap.c',
                                #'lua.c',
                                #'shadow_los.c',
                                'entity.c',
                                'component.c',
                                'mempool.c',
                                'mempool_ext.c',
                                'message.c'
                                ])

env.ParseConfig('sdl-config --cflags')
#env.ParseConfig('pkg-config lua5.1 --cflags')

env.Append(CCFLAGS = ['-g', '-Wall'])
env.Append(CPPPATH = ['/usr/local/include'])
env.Append(LIBPATH = ['/usr/local/lib'])
#env.Append(LIBS = ['asound', 'portaudio', 'pthread', 'rt'])
    
Default(t)

#include "lapis.h"

#include <lua.h>
#include <lualib.h>
#include <lauxlib.h>

struct lapis_lua
{
    lua_State *lua;
};

extern int luaopen_lapis(lua_State* lua); // declare the wrapped module

lapis_lua_t *lua_scripting_init()
{
    lapis_lua_t *ll = malloc(sizeof(*ll));
    ll->lua = lua_open();
    luaopen_base(ll->lua);
    luaL_openlibs(ll->lua);
    luaopen_lapis(ll->lua);
    
    return ll;
}

int lua_scripting_run_file(lapis_lua_t *ll, char *filename)
{
    if (luaL_loadfile(ll->lua, filename) == 0)
    {
        if(lua_pcall(ll->lua,0,0,0) != 0)
        {
            ERROR("problem executing lua code: %s\n", filename);
            ERROR("%s\n", lua_tostring(ll->lua, -1));

            return 1;
        }
    }
    else
    {
        ERROR("unable to load %s\n", filename);
        
        return 1;
    }

    return 0;
}

void lua_scripting_destroy(lapis_lua_t *ll)
{
    lua_close(ll->lua);
}

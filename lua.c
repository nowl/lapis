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
    luaopen_lapis(ll->lua);
    
    return ll;
}

void lua_scripting_run_file(lapis_lua_t *ll, char *filename)
{
    if (luaL_loadfile(ll->lua, filename) == 0)
        lua_pcall(ll->lua,0,0,0);
    else
        WARN("unable to load %s\n", filename);
}

void lua_scripting_destroy(lapis_lua_t *ll)
{
    lua_close(ll->lua);
}

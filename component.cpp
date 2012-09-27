#include <algorithm>

#include "component.hpp"
#include "entity.hpp"
#include "message.hpp"
#include "hash.hpp"

Component::ResponderListType Component::responderList;

Component::Component(ResponderFunc func)
    : _responderFunc(func)
{}

Component::~Component()
{
    auto iter = responderList.begin();
    for(; iter != responderList.end(); iter++)
    {
        std::remove(iter->second.begin(),
                    iter->second.end(),
                    this);
    }
}

bool
Component::respond(Message *message)
{
    auto iter = _entities.begin();
    for(; iter != _entities.end(); ++iter)
    {
        bool result = _responderFunc(message, *iter);
        if(result)
            return true;
    }
    
    return false;
}

void Component::addResponderType(std::string type)
{
    auto hash = Hash::hashString(type.c_str());
    responderList[hash].push_back(this);
}

void Component::removeResponderType(std::string type)
{
    auto hash = Hash::hashString(type.c_str());
    std::remove(responderList[hash].begin(),
                responderList[hash].end(),
                this);
}

void Component::addEntity(Entity *entity)
{
    _entities.push_back(entity);
}

void Component::removeEntity(Entity *entity)
{
    std::remove(_entities.begin(),
                _entities.end(),
                entity);
}

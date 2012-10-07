#include <algorithm>

#include "component.hpp"
#include "message.hpp"
#include "hash.hpp"

Component::ResponderListType Component::responderList;

Component::Component()
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

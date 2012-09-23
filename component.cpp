#include "component.hpp"
#include "entity.hpp"
#include "message.hpp"

bool
Component::respond(std::weak_ptr<Message> message,
                   std::weak_ptr<Entity> entity)
{
    return _responderFunc(message, entity);
}

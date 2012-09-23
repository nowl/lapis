#ifndef __COMPONENT_HPP__
#define __COMPONENT_HPP__

#include <memory>
#include <map>
#include <string>

class Message;
class Entity;

class Component {
public:
    typedef std::function< bool(std::weak_ptr<Message>, std::weak_ptr<Entity>) > ResponderFunc;
    
    bool respond(std::weak_ptr<Message> message, std::weak_ptr<Entity> entity);
    
private:
    ResponderFunc _responderFunc;
};

#endif  // __COMPONENT_HPP__

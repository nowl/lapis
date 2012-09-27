#ifndef __COMPONENT_HPP__
#define __COMPONENT_HPP__

#include <memory>
#include <vector>
#include <map>
#include <string>
#include <unordered_map>

class Message;
class Entity;

class Component {
public:
    typedef std::function< bool(Message *message, Entity *entity) > ResponderFunc;
    
    Component(ResponderFunc func);
    ~Component();
    // TODO: add copy and assignment op
    
    bool respond(Message *message);

    void addResponderType(std::string type);
    void removeResponderType(std::string type);

    void addEntity(Entity *entity);
    void removeEntity(Entity *entity);
    
    typedef std::unordered_map<unsigned long, std::vector<Component*> > ResponderListType;
    static ResponderListType responderList;

private:
    ResponderFunc _responderFunc;
    std::vector<Entity*> _entities;
};

#endif  // __COMPONENT_HPP__

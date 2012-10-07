#ifndef __COMPONENT_HPP__
#define __COMPONENT_HPP__

#include <memory>
#include <vector>
#include <map>
#include <string>
#include <unordered_map>

class Message;

class Component {
public:
    Component();
    virtual ~Component();
    // TODO: add copy and assignment op
    
    virtual bool respond(Message *message) = 0;

    void addResponderType(std::string type);
    void removeResponderType(std::string type);

    typedef std::unordered_map<unsigned long, std::vector<Component*> > ResponderListType;
    static ResponderListType responderList;
};

#endif  // __COMPONENT_HPP__

#include "message.hpp"

std::vector< std::shared_ptr<Message> > messages;

void processMessage(Message *message)
{
    auto components = Component::responderList[message->type];
    for(auto iter = components.begin(); iter != components.end(); ++iter)
    {
        (*iter)->respond(message, NULL);
    }
}

#ifndef __MESSAGE_HPP__
#define __MESSAGE_HPP__

#include <tr1/memory>
#include <string>
#include <cstdio>
#include <vector>

#include "noncopyable.hpp"

class Entity;
class Message;

// maintains list of messages to queue up for synchronous processing
std::vector< std::shared_ptr<Message> > messages;

class Message : private noncopyable
{
public:
    enum DeliveryType { SYNC, ASYNC };

    class IPayload
    {
    public:
        virtual ~IPayload() {};
    };

    Entity *source;
    unsigned int type;
    std::shared_ptr<IPayload> payload;

    template <class T>
    static void send(Entity *source,
                     unsigned int type,
                     const T &payload,
                     DeliveryType deliveryType);
};

template <class T>
void Message::send(Entity *source,
                   unsigned int type,
                   const T &payload,
                   DeliveryType deliveryType)
{
    if(deliveryType == ASYNC)
    {
        Message message;
        message.source = source;
        message.type = type;
        message.payload = std::make_shared<T>(payload);
        // process message here
        //processMessage(&message);
        //printf("sending async\n");
    }
    else                    // SYNC
    {
        auto message = std::make_shared<Message>();
        message->source = source;
        message->type = type;
        message->payload = std::make_shared<T>(payload);
        // add to process message list           
        printf("sending sync\n");
        messages.push_back(message);
    }
}




/*
class FloatPayload : public Message::IPayload
{
public:
    float a;
};

void callFunction(std::function< void(std::string) > f)
{
    f("haha");
}

void processMessage(Message *message)
{
    printf("processing message of type %d\n", message->type);
    auto p = std::static_pointer_cast<FloatPayload>(message->payload);
    //printf("float payload of %f\n", p->a);

    auto func = [&] (std::string str) {printf("float %s payload of %f\n", str.c_str(), p->a); };
    callFunction(func);
}

int main()
{
    FloatPayload p;
    p.a = 24;

    Message::send<FloatPayload>(NULL, 4, p, Message::ASYNC);

    return 0;
}
*/

#endif  // __MESSAGE_HPP__

#ifndef __MESSAGE_HPP__
#define __MESSAGE_HPP__

#include <tr1/memory>
#include <string>
#include <cstdio>
#include <vector>

#include "noncopyable.hpp"
#include "component.hpp"
#include "hash.hpp"

class Entity;
class Message;

// maintains list of messages to queue up for synchronous processing
extern std::vector< std::shared_ptr<Message> > messages;

void processMessage(Message *message);

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
    unsigned long type;
    std::shared_ptr<IPayload> payload;

    template <class T>
    static void send(Entity *source,
                     const char *type,
                     const T &payload,
                     DeliveryType deliveryType);

    template <class T>
    static void send(Entity *source,
                     unsigned long type,
                     const T &payload,
                     DeliveryType deliveryType);
};

template <class T>
void Message::send(Entity *source,
                   const char *type,
                   const T &payload,
                   DeliveryType deliveryType)
{
    send<T>(source, Hash::hashString(type), payload, deliveryType);
}

template <class T>
void Message::send(Entity *source,
                   unsigned long type,
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
        processMessage(&message);
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


int main()
{
    FloatPayload p;
    p.a = 24;

    Message::send<FloatPayload>(NULL, 4, p, Message::ASYNC);

    return 0;
}
*/

#endif  // __MESSAGE_HPP__

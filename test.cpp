#include <cstdio>

#include "entity.hpp"
#include "message.hpp"

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

int main(int argc, char *argv[])
{
    float a = testing(14);
    printf("%f\n", a);
   
    FloatPayload p;
    p.a = 24;

    Message::send<FloatPayload>(NULL, 4, p, Message::ASYNC);

    return 0;
}

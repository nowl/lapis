#include <cstdio>

#include "entity.hpp"
#include "message.hpp"
#include "sdl_driver.hpp"
#include "engine.hpp"
#include "component.hpp"
#include "log.hpp"

class FloatPayload : public Message::IPayload
{
public:
    float a;
};

void callFunction(std::function< void(std::string) > f)
{
    f("haha");
}

/*
void processMessage(Message *message)
{
    printf("processing message of type %d\n", message->type);
    auto p = std::static_pointer_cast<FloatPayload>(message->payload);
    //printf("float payload of %f\n", p->a);

    auto func = [&] (std::string str) {printf("float %s payload of %f\n", str.c_str(), p->a); };
    callFunction(func);
}
*/

bool comp1_responder(Message *message, Entity *entity)
{
    //auto p = std::static_pointer_cast<FloatPayload>(message->payload);

    LOG("here\n");
    return false;
}

int main(int argc, char *argv[])
{
    Engine e;
    //float a = testing(14);
    //printf("%f\n", a);
    //const std::unique_ptr<SDLDriver>& sdl = e.getSDLDriver();
       
    //FloatPayload p;
    //p.a = 24;

    //Message::send<FloatPayload>(NULL, 4, p, Message::ASYNC);

    Component comp1(comp1_responder);
    comp1.addResponderType("sdl-event");

    e.run();

    return 0;
}

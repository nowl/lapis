#include <SDL.h>

#include <cstdio>

#include "entity.hpp"
#include "message.hpp"
#include "sdl_driver.hpp"
#include "engine.hpp"
#include "component.hpp"
#include "log.hpp"
#include "hash.hpp"

static Engine engine;

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
    if(message->type == Hash::hashString("ui-event"))
    {
        auto p = std::static_pointer_cast<SDLEventPayload>(message->payload);
        
        if(p->event.type == SDL_QUIT ||
           (p->event.type == SDL_KEYDOWN && p->event.key.keysym.sym == SDLK_q))
            engine.quit();        
    }

    return false;
}

int main(int argc, char *argv[])
{
    engine.getSDLDriver()->setVideoMode(1024, 768);
    //float a = testing(14);
    //printf("%f\n", a);
    //const std::unique_ptr<SDLDriver>& sdl = e.getSDLDriver();
       
    //FloatPayload p;
    //p.a = 24;

    //Message::send<FloatPayload>(NULL, 4, p, Message::ASYNC);

    Component comp1(comp1_responder);
    comp1.addResponderType("ui-event");

    engine.run();

    return 0;
}

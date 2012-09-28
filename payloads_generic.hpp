#ifndef __PAYLOADS_GENERIC_HPP__
#define __PAYLOADS_GENERIC_HPP__

#include "message.hpp"

class IntPayload : public Message::IPayload
{
public:
    IntPayload(int payload)
        : payload(payload)
    {}

    int payload;
};

class FloatPayload : public Message::IPayload
{
public:
    FloatPayload(float payload)
        : payload(payload)
    {}

    float payload;
};


#endif  // __PAYLOADS_GENERIC_HPP__

#include <climits>
#include <ctime>

#include "random.hpp"

static struct {
    unsigned long a, b, c, d;
} CTX;

#define ROT(x, k) ( ( (x)<<(k)) | ((x)>>(32-(k)) ) )

void Random::init()
{
    init(time(NULL));
}

void Random::init(unsigned long seed)
{
    unsigned long i;
    CTX.a = 0xf1ea5eed;
    CTX.b = CTX.c = CTX.d = seed;
    for (i=0; i<20; ++i)
        ul();
}

unsigned long Random::ul()
{
    unsigned long e = CTX.a - ROT(CTX.b, 27);
    CTX.a = CTX.b ^ ROT(CTX.c, 17);
    CTX.b = CTX.c + CTX.d;
    CTX.c = CTX.d + e;
    CTX.d = e + CTX.a;
    return CTX.d;
}

float Random::f()
{
    unsigned long v = ul();
    return (float)v/ULONG_MAX;
}

int Random::intMinMax(int min, int max)
{
    int span = max - min + 1;
    unsigned long v = ul() % span;
    return v + min;
}

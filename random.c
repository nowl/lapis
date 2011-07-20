#include "lapis.h"

static struct {
    unsigned long a, b, c, d;
} CTX;

#define ROT(x, k) ( ( (x)<<(k)) | ((x)>>(32-(k)) ) )

unsigned long
random_ul()
{
    unsigned long e = CTX.a - ROT(CTX.b, 27);
    CTX.a = CTX.b ^ ROT(CTX.c, 17);
    CTX.b = CTX.c + CTX.d;
    CTX.c = CTX.d + e;
    CTX.d = e + CTX.a;
    return CTX.d;
}

static void
init(unsigned long seed)
{
    unsigned long i;
    CTX.a = 0xf1ea5eed;
    CTX.b = CTX.c = CTX.d = seed;
    for (i=0; i<20; ++i)
        random_ul();
}

void
random_init()
{
    init(time(NULL));
}

float
random_float()
{
    unsigned long v = random_ul();
    return (float)v/ULONG_MAX;
}

int
random_int_min_max(int min, int max)
{
    int span = max - min + 1;
    unsigned long v = random_ul() % span;
    return v + min;
}

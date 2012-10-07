#ifndef __RANDOM_HPP__
#define __RANDOM_HPP__

class Random {
public:
    static void init();
    static void init(unsigned long seed);
    static unsigned long ul();
    static float f();
    static int intMinMax(int min, int max);
};

#endif  // __RANDOM_HPP__

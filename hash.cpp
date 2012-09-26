#include "hash.hpp"

static unsigned long
jenkins_hash_char(const char* key)
{
    unsigned long hash = 0;
    const char *str = key;
    char c;

    //int loop = 0;
    //while (((c = *str++) != 0) && (loop++ < MAX_LOOPS)) {
    while ((c = *str++) != 0) {
        hash += c;
        hash += (hash << 10);
        hash ^= (hash >> 6);
    }
    hash += (hash << 3);
    hash ^= (hash >> 11);
    hash += (hash << 15);
 
    return hash;
}

unsigned long
Hash::hashString(const char *str)
{
    return jenkins_hash_char(str);
}

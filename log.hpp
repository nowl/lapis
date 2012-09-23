#ifndef __LOG_H__
#define __LOG_H__

#include <cstdio>

#ifdef NDEBUG
# define LOG
#else
# define LOG(...) { fprintf(stdout, "[L] %s:%d -> ", __FILE__, __LINE__); fprintf(stdout, __VA_ARGS__); }
#endif

#define WARN(...) { fprintf(stdout, "[W] %s:%d -> ", __FILE__, __LINE__); fprintf(stdout, __VA_ARGS__); }
#define ERROR(...) { fprintf(stdout, "[E] %s:%d -> ", __FILE__, __LINE__); fprintf(stdout, __VA_ARGS__); }

#endif  // __LOG_H__

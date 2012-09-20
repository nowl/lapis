#ifndef __NONCOPYABLE_HPP__
#define __NONCOPYABLE_HPP__

class noncopyable
{
protected:
    noncopyable() {}
    ~noncopyable() {}
private:
    noncopyable(const noncopyable& other);
    noncopyable operator=(const noncopyable& other);
};

#endif  // __NONCOPYABLE_HPP__

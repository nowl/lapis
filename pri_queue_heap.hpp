#ifndef __PRI_QUEUE_HEAP_HPP__
#define __PRI_QUEUE_HEAP_HPP__

#include <queue>
#include <vector>
#include <ostream>

template<class T, 
         class Container=std::vector<T>, 
         class Compare=std::less<T>,
         class Equal=std::less<T> >
class PriQueueMutableHeap
{
private:
    struct HackedPriorityQueue : public std::priority_queue<T, Container, Compare> {
        Container& getUnderlyingContainer() {
            return this->c;
        }
        
        const Container& getUnderlyingContainer() const {
            return this->c;
        }
    };
    
public:    
    bool empty() const {
        return _queue.empty();
    }
    
    T pop() {
        T result = _queue.top();
        _queue.pop();
        return result;
    }

    void push(const T& value) {
        _queue.push(value);
    }

    int size() const {
        return _queue.size();
    }
    
    void remove(const T& value) {
        Container &c = _queue.getUnderlyingContainer();
        typename Container::iterator iter = c.begin();
        for(; iter != c.end(); iter++)
            if(_equal(*iter, value)) {
                c.erase(iter);
                break;
            }
    }
    
    bool find(const T& value, T& result) {
        bool found = false;
        Container &c = _queue.getUnderlyingContainer();
        typename Container::iterator iter = c.begin();
        for(; iter != c.end(); iter++)
            if(_equal(*iter, value)) {
                result = *iter;
                found = true;
                break;
            }
        
        return found;
    }
    
private:
    HackedPriorityQueue _queue;
    Equal _equal;
    
    /*
    template<class A, class B, class C, class D>
    friend std::ostream& operator<<(std::ostream& out, PriQueueMutableHeap<A,B,C,D> &heap);
    */
};

/*
template<class A, class B, class C, class D>
std::ostream& operator<<(std::ostream& out,
                         PriQueueMutableHeap<A,B,C,D> &heap)
{
    auto iter = heap._queue.getUnderlyingContainer().begin();
    for(; iter != heap._queue.getUnderlyingContainer().end(); ++iter)
        out << **iter << ", ";
    return out;
}
*/

#endif	// __PRI_QUEUE_HEAP_HPP__

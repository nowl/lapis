#ifndef __ASTAR_HPP__
#define __ASTAR_HPP__

#include <iostream>
#include <ostream>
#include <vector>
#include <memory>
#include <algorithm>

#include "pri_queue_heap.hpp"

static const int MAX_OPEN_LIST = 500;

template<class MapType, class ElemType>
class Node {
public:
    Node() {}

    Node(const MapType &m, const ElemType &a) : _elem(a), _map(&m), mParent(NULL), mCostFromStart(0), mTotalCost(0) {}

    bool operator>(const Node& node) const {
        return mTotalCost > node.mTotalCost;
    }

    bool operator==(const Node& node) const {
        return node._elem == _elem;
    }

    const ElemType getElem() const { return _elem; }

private:
    ElemType _elem;
    const MapType* _map;
    
    /*
    template<class A, class B>
    friend std::ostream& operator<<(std::ostream& out, Node<A, B> &elem);
    */

public:
    Node *mParent;
    float mCostFromStart;
    float mTotalCost;
};

/*
template<class MapType, class ElemType>
std::ostream& operator<<(std::ostream& out, Node<MapType, ElemType> &elem)
{
    out << elem._elem;
    return out;
}
*/

template<class A, class B>
struct openListGtrCompare : public std::binary_function<A, B, bool> {
    bool operator()(const A& a, const B& b) {
        return *a > *b;
    }
};

template<class A, class B>
struct openListEqualCompare : public std::binary_function<A, B, bool> {
    bool operator()(const A& a, const B& b) {
        return *a == *b;
    }
};

// MapType must support the following operations:
//   - getCost(ElemType) -> float
//   - getHeuristicDistance(ElemType, ElemType) -> float
//   - getAdjacencies(ElemType) -> vector<ElemType>
template<class MapType, class ElemType>
std::vector<ElemType>
best_path(const MapType& map, const ElemType& start, const ElemType& end)
{
    typedef Node<MapType, ElemType> LocalNode;

    std::vector< std::unique_ptr<LocalNode> > fullNodeList;

    typedef PriQueueMutableHeap<LocalNode*, std::vector<LocalNode*>,
                                openListGtrCompare<LocalNode*, LocalNode*>,
                                openListEqualCompare<LocalNode*, LocalNode*> >
    OpenListPriorityQueueImpl;

    std::vector<ElemType> results;
    OpenListPriorityQueueImpl openList;
    std::vector<LocalNode*> closedList;

    LocalNode startNode(map, start), endNode(map, end);

    // fill in the start location cost since this was passed in as a location
    // with no cost associated with it
    startNode.mCostFromStart = map.getCost(start);

    bool reachedEndNode = false;
    LocalNode pathEndNode;

    // add start node to the open list

    auto newNode = std::unique_ptr<LocalNode>(new LocalNode(startNode));
    openList.push(newNode.get());
    fullNodeList.push_back( std::move(newNode) );

    // while the open list is not empty

    while(!openList.empty() && openList.size() < MAX_OPEN_LIST) {
        
        // debug
        /*
        std::cout << "--- next open ---" << std::endl;
        std::cout << "open list: " << std::endl;
        std::cout << openList << std::endl;
        std::cout << "closed list: " << std::endl;
        auto iter_tmp = closedList.begin();
        for(; iter_tmp != closedList.end(); ++iter_tmp)
            std::cout << **iter_tmp << ", ";
        std::cout << std::endl;
        */

        // grab current node with lowest F value
        
        auto node = openList.pop();
    
        // add node to the closed list

        closedList.push_back(node);
        
        // debug
        //std::cout << "working node: " << *node << std::endl;        

        // if node is the endNode then we're done
        if(*node == endNode) {
            reachedEndNode = true;
            pathEndNode = *node;
            break;
        }

        // get adjacencies
        std::vector<ElemType> adjacencies;
        map.getAdjacencies(node->getElem(), adjacencies);

        // debug
        /*
        std::cout << "adjacencies: " << std::endl;        
        auto iter_tmp2 = adjacencies.begin();
        for(; iter_tmp2 != adjacencies.end(); ++iter_tmp2)
            std::cout << *iter_tmp2 << std::endl;
        */

        // loop through each element
        auto iter = adjacencies.begin();
        for(; iter != adjacencies.end(); iter++)
        {
            auto r = *iter;
            //cout << "-------- loop start ------------" << endl;

            auto n1_ref = std::unique_ptr<LocalNode>(new LocalNode(map, r));
            auto n1 = n1_ref.get();
            fullNodeList.push_back( std::move(n1_ref) );
            
            // set parent to node
            n1->mParent = node;

            // find heuristic cost to goal
            float h = map.getHeuristicCost(n1->getElem(), endNode.getElem());

            // get estimated cost from start node
            float g = node->mCostFromStart + map.getCost(n1->getElem());

            // get estimated total cost from start to end through this node
            float f = g + h;
            n1->mTotalCost = f;
            n1->mCostFromStart = g;

            // check if n1 is already in the open list and if so then if
            // it's better or the same then continue

            // TOOD: rework this to just return null
            LocalNode* tmp;
            if(openList.find(n1, tmp))
                if(tmp->mTotalCost <= n1->mTotalCost)
                    continue;
	
            // check if n1 is already in the closed list and if so then if
            // it's better or the same then continue
            bool found = false;
            auto i2 = closedList.begin();
            while(i2 != closedList.end()) {
                LocalNode *nn = *i2;
                if(*nn == *n1) {
                    if(nn->mTotalCost <= n1->mTotalCost)
                        found = true;
                    break;
                }
                i2++;
            }
            if(found)
                continue;

            // debug
            /*
            std::cout << "open list (before erase): " << std::endl;
            std::cout << "  " << openList << std::endl;
            */

            // remove occurances of n1 from both open and closed lists if it exists
            // in either list
            openList.remove(n1);

            i2 = closedList.begin();
            while(i2 != closedList.end()) {
                LocalNode *nn = *i2;
                if(*nn == *n1) {
                    //cout << "erasing: " << *nn << endl;
                    closedList.erase(i2);
                    break;
                }
                i2++;
            }

            // debug
            //cout << "open list (after erase): " << endl;
            //cout << "  " << openList << endl;

            // add n1 to the open list
            //cout << "n1 parent: " << hex << (int)(n1->mParent->mParent) << endl;
            //cout << "adding to open list: " << *n1 << endl;
            openList.push(n1);

            //cout << "openlist size = " << openListNodes.size() << endl;
            //cout << "closedlist size = " << closedList.size() << endl;
        }
    }

    // backtrack and copy results into the results buffer
    if(reachedEndNode) {
        LocalNode *n = &pathEndNode;
        while(n) {
            //cout << "here: " << hex << (int)n << endl;
            results.push_back(n->getElem());
            n = n->mParent;
        }
    }

    // return it in the forward order
    reverse(results.begin(), results.end());

    return results;
}

#endif // __ASTAR_HPP__

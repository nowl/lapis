#include "entity.hpp"

std::shared_ptr<Entity::IMeta>
Entity::getMeta(std::string val)
{
    if(_meta.find(val) == end(_meta))
        return std::shared_ptr<IMeta>();
    
    return _meta[val];
}

/*
class PositionMeta : public Entity::IMeta
{
public:
    float p1, p2;
};

float testing(float b)
{
    Entity e1;

    PositionMeta pm;
    pm.p1 = 1;
    pm.p2 = 5;
    e1.setMeta<PositionMeta>("testing", pm);

    pm = PositionMeta();
    e1.setMeta("testing2", pm);

    auto p = std::static_pointer_cast<PositionMeta>(e1.getMeta("testing"));
    p->p2 = b;

    p = std::static_pointer_cast<PositionMeta>(e1.getMeta("alsdjkfalkdjf"));
    if(!p)
    {
        printf("not found\n");
    
        return 0;
    }

    return p->p2;
}
*/

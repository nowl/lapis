#ifndef __ENTITY_HPP__
#define __ENTITY_HPP__

#include <tr1/memory>
#include <map>
#include <string>

class Entity {
public:
    class IMeta {
    public:
        virtual ~IMeta() {}
    };

    template<class U>
    void setMeta(std::string val, const U &meta);

    std::shared_ptr<IMeta> getMeta(std::string val);

private:
    typedef std::map<std::string, std::shared_ptr<IMeta> > MetaMap;
    MetaMap _meta;
};

template<class U>
void Entity::setMeta(std::string val, const U &meta)
{
    _meta[val] = std::make_shared<U>(meta);
}

#endif  // __ENTITY_HPP__

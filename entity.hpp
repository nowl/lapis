#ifndef __ENTITY_HPP__
#define __ENTITY_HPP__

#include <tr1/memory>
#include <map>
#include <string>

extern "C" {
    float testing(float b);
}

class Entity {
public:
    class IMeta {
    public:
        virtual ~IMeta() {}
    };

    template<class U>
    void setMeta(std::string val, const U &meta)
    {
        printf("setting value %s\n", val.c_str());
/*
        MetaMap::iterator iter;
        if( (iter = _meta.find(val)) != _meta.end() )
            delete iter->second;
*/

        _meta[val] = std::make_shared<U>(meta);
    }

    std::shared_ptr<IMeta> getMeta(std::string val)
    {
        if(_meta.find(val) == end(_meta))
        {
            printf("returning a null\n");
            return std::shared_ptr<IMeta>();
        }
    
        return _meta[val];
    }

private:
    typedef std::map<std::string, std::shared_ptr<IMeta> > MetaMap;
    MetaMap _meta;
};

#endif  // __ENTITY_HPP__

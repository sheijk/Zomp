
#include <map>
#include <string>

// A simple C interface to std::map
extern "C"
{
    typedef std::map<std::string, void*> StringMap;

    StringMap* StringMap_new()
    {
        return new StringMap();
    }

    void StringMap_delete(StringMap* map)
    {
        delete map;
    }

    void StringMap_insert(StringMap* map, char* key, void* value)
    {
        if( map ) {
            map->insert( std::make_pair(std::string(key), value) );
        }
    }

    void StringMap_remove(StringMap* map, char* key)
    {
        if( map ) {
            map->erase( map->find(key) );
        }
    }

    void* StringMap_find(StringMap* map, char* key)
    {
        StringMap::iterator iter = map->find(key);
        if( iter != map->end() ) {
            return iter->second;
        }
        else {
            return NULL;
        }
    }

    void StringMap_debugPrint(StringMap* map)
    {
        if( map ) {
            StringMap::iterator current = map->begin(), end = map->end();
            printf( "Debug dump of Hashtable:\n" );
            for( ; current != end; ++current ) {
                printf( "  %s -> %p\n", current->first.c_str(), current->second );
            }
            fflush( stdout );
        }
    }
}


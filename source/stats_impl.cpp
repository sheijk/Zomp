///
/// Virtual machine implementation. Contains code interacting with
/// LLVM. Provides services to evaluate LLVM code, create and redefine
/// functions, types and globals
///

#include "zomputils.h"
#include "stats_impl.h"

#include <string.h>
#include <vector>
#include <list>

namespace {
enum Constants
{
    MAX_COUNTER_NAME_LENGTH = 1024
};

static bool isValidName(const char* name)
{
    return name != NULL;
};

static void printIndent(int indent)
{
    for ( int ii = 0; ii < indent; ++ii )
    {
        printf(" ");
    }
}

static const char* c_str(const std::vector<char>& name)
{
    if(name.size() > 0)
        return &name[0];
    else
        return "";
}

} // anonymous namespace

extern "C"
{

struct Counter
{
private:
    std::vector<char> name_;
    void* userData_;
    u32 fractionalDigits_;
    CounterQueryFunction query_;
    Section* parent_;
    Counter* next_;

public:
    Counter(Section* parent, const char* name, void* userData, u32 fractionalDigits, CounterQueryFunction query)
    {
        parent_ = parent;
        name_.assign(name, name + strnlen(name, MAX_COUNTER_NAME_LENGTH));
        userData_ = userData;
        fractionalDigits_ = fractionalDigits;
        query_ = query;
        next_ = NULL;
    }

    Counter* next()
    {
        return next_;
    }

    void setNext(Counter* counter)
    {
        next_ = counter;
    }

    const char* name()
    {
        return c_str(name_);
    }

    i32 fractionalDigits()
    {
        return fractionalDigits_;
    }

    i64 query()
    {
        return query_(this, userData_);
    }
};

struct Section
{
private:
    std::vector<char> name_;
    typedef std::list<Counter*> CounterList;
    CounterList counters_;
    typedef std::list<Section*> SectionList;
    SectionList sections_;
    Section* parent_;
    Section* next_;

    void add(Section* newSection)
    {
        if(sections_.size() > 0)
        {
            sections_.back()->next_ = newSection;
        }
        sections_.push_back(newSection);
    }

public:
    explicit Section(Section* parent, const char* name) : parent_(parent), next_(NULL)
    {
        ZMP_ASSERT(isValidName(name),);
        ZMP_ASSERT(parent != NULL || name[0] == '\0',);

        if(parent_ != NULL)
            parent_->add(this);

        name_.assign(name, name + strnlen(name, MAX_COUNTER_NAME_LENGTH));
    }

    ~Section()
    {
        const CounterList::iterator counters_End = counters_.end();
        for( CounterList::iterator counter = counters_.begin(); counter != counters_End; ++counter ) {
            delete *counter;
        }

        const SectionList::iterator sections_End = sections_.end();
        for( SectionList::iterator section = sections_.begin(); section != sections_End; ++section ) {
            delete *section;
        }
    }

    Section* createSection(const char* name)
    {
        return new Section(this, name);
    }

    Counter* createCounter(const char* name, u32 fractionalDigits, void* userData, CounterQueryFunction query)
    {
        Counter* counter = new Counter(this, name, userData, fractionalDigits, query);
        if(counters_.size() > 0)
        {
            counters_.back()->setNext(counter);
        }
        counters_.push_back(counter);
        return counter;
    }

    Section* next()
    {
        if(this != NULL)
            return next_;
        else
            return NULL;
    }

    Section* firstChildSection()
    {
        if(this != NULL && sections_.size() > 0)
            return sections_.front();
        else
            return NULL;
    }

    Counter* firstCounter()
    {
        if(this != NULL && counters_.size() > 0)
            return counters_.front();
        else
            return NULL;
    }

    void printReport(int indent)
    {
        if(name_.size() > 1) {
            printIndent(indent);
            printf("%s:\n", c_str(name_));
        }

        for(Counter* counter = firstCounter(); counter != NULL; counter = counter->next())
        {
            printIndent(indent + 4);
            i64 value = counter->query();
            u32 fractionalDigits = counter->fractionalDigits();
            if(fractionalDigits == 0) {
                printf("%lld - %s\n", value, counter->name());
            }
            else {
                i64 mask = (~(i64)-1) >> (64 - fractionalDigits);
                i64 fract = value & mask;
                i64 nonFractional = (value - fract) >> counter->fractionalDigits();
                printf("%lld.%.*lld - %s\n", nonFractional, counter->fractionalDigits(), fract, counter->name());
            }
        }

        for(Section* child = firstChildSection(); child != NULL; child = child->next())
        {
            child->printReport(indent + 4);
        }
    }
};

static Section mainSection(NULL, "");

Section* statsMainSection()
{
    return &mainSection;
}

Section* statsCreateSection(Section* parent, const char* name)
{
    return parent->createSection(name);
}

// void DeleteSection(Section* section)
// {
//     section->Delete();
// }

Counter* statsCreateCounter(Section* parent, const char* name, u32 fractionalDigits, void* userData, CounterQueryFunction query)
{
    return parent->createCounter(name, fractionalDigits, userData, query);
}

static i64 ReturnIntValue(Counter*, void* userData)
{
    int* ptr = (int*)userData;
    return i64(*ptr);
}

Counter* statsCreateCounterForValue(Section* parent, const char* name, u32 fractionalDigits, int* ptr)
{
    return parent->createCounter(name, fractionalDigits, ptr, ReturnIntValue);
}

// void DeleteCounter(Counter* counter)
// {
//     counter->Delete();
// }

Section* statsNextSection(Section* section)
{
    return section->next();
}

Section* statsFirstChildSection(Section* section)
{
    return section->firstChildSection();
}

Counter* statsFirstCounter(Section* section)
{
    return section->firstCounter();
}

Counter* statsNextCounter(Counter* counter)
{
    return counter->next();
}

const char* statsCounterName(Counter* counter)
{
    return counter->name();
}

i64 statsCounterQuery(Counter* counter)
{
    return counter->query();
}

i32 statsCounterFractionalDigits(Counter* counter)
{
    return counter->fractionalDigits();
}

void statsPrintReport(int indent)
{
    printf("Zomp statistics\n");
    statsMainSection()->printReport(indent);
}

} // extern "C"


///
/// Virtual machine implementation. Contains code interacting with
/// LLVM. Provides services to evaluate LLVM code, create and redefine
/// functions, types and globals
///

#include "zomputils.h"
#include "stats_impl.h"
#include "zompvm_caml.h"

#include <string.h>
#include <vector>
#include <list>

namespace {
enum Constants
{
    MAX_COUNTER_NAME_LENGTH = 1024,
    MAX_CAML_INT = (1 << 30),
};

static bool isValidName(const char* name)
{
    return name != NULL;
};

static void printIndent(FILE* out, int indent)
{
    for ( int ii = 0; ii < indent; ++ii )
    {
        fprintf(out, " ");
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
protected:
    std::vector<char> name_;
    void* userData_;
    u32 fractionalDigits_;
    Section* parent_;
    Counter* next_;

    Counter(Section* parent, const char* name, u32 fractionalDigits, void* userData)
    {
        parent_ = parent;
        name_.assign(name, name + strnlen(name, MAX_COUNTER_NAME_LENGTH) + 1);
        userData_ = userData;
        fractionalDigits_ = fractionalDigits;
        next_ = NULL;
    }

public:
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

    virtual void printValue(FILE* out) = 0;
};

struct CounterInt : public Counter
{
    CounterQueryFunction query_;
public:

    CounterInt(Section* parent, const char* name, u32 fractionalDigits, void* userData, CounterQueryFunction query)
        : Counter(parent, name, fractionalDigits, userData)
    {
        query_ = query;
    }

    virtual void printValue(FILE* out)
    {
        i64 value = query_(this, userData_);
        if(fractionalDigits_ == 0) {
            fprintf(out, "%lld", value);
        }
        else {
            double fraction = double(value) / double(1LL << fractionalDigits_);
            // smallest value is 1/(2**fractionalDigits) so we need fractionalDigits
            // number of digits precision.
            fprintf(out, "%.*f", fractionalDigits_, fraction);
        }
    }
};

struct CounterFloat : public Counter
{
    CounterQueryFunctionFloat query_;
public:

    CounterFloat(Section* parent, const char* name, u32 fractionalDigits, void* userData, CounterQueryFunctionFloat query)
        : Counter(parent, name, fractionalDigits, userData)
    {
        query_ = query;
    }

    virtual void printValue(FILE* out)
    {
        fprintf(out, "%.*f", fractionalDigits_, query_(this, userData_));
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

        name_.assign(name, name + strnlen(name, MAX_COUNTER_NAME_LENGTH) + 1);
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

    void addCounter(Counter* counter)
    {
        if(counters_.size() > 0)
        {
            counters_.back()->setNext(counter);
        }
        counters_.push_back(counter);
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

    Section* findNamedChild(const char* name)
    {
        for(Section* section = firstChildSection(); section != NULL; section = section->next())
        {
            if(strncmp(c_str(section->name_), name, MAX_COUNTER_NAME_LENGTH) == 0)
            {
                return section;
            }
        }

        return NULL;
    }

    void printReport(FILE* out, int indent)
    {
        if(name_.size() > 1) {
            printIndent(out, indent);
            fprintf(out, "%s:\n", c_str(name_));
        }

        for(Counter* counter = firstCounter(); counter != NULL; counter = counter->next())
        {
            printIndent(out, indent + 4);
            counter->printValue(out);
            fprintf(out, " - %s\n", counter->name());
        }

        for(Section* child = firstChildSection(); child != NULL; child = child->next())
        {
            child->printReport(out, indent + 4);
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

Counter* statsCreateIntCounter(Section* parent, const char* name, u32 fractionalBits, void* userData, CounterQueryFunction query)
{
    Counter* counter = new CounterInt(parent, name, fractionalBits, userData, query);
    parent->addCounter(counter);
    return counter;
}

Counter* statsCreateFloatCounter(Section* parent, const char* name, u32 fractionalDigits, void* userData, CounterQueryFunctionFloat query)
{
    Counter* counter = new CounterFloat(parent, name, fractionalDigits, userData, query);
    parent->addCounter(counter);
    return counter;
}

static i64 ReturnIntValue(Counter*, void* userData)
{
    int* ptr = (int*)userData;
    return i64(*ptr);
}

Counter* statsCreateCounterForValue(Section* parent, const char* name, u32 fractionalDigits, int* ptr)
{
    Counter* counter = new CounterInt(parent, name, fractionalDigits, ptr, ReturnIntValue);
    parent->addCounter(counter);
    return counter;
}

void statsCreateNamedSection(const char* sectionName)
{
    if(statsMainSection()->findNamedChild(sectionName) == NULL) {
        statsCreateSection(statsMainSection(), sectionName);
    }
}

static i64 ReturnCamlCounterValue(Counter*, void* userData)
{
    ptrdiff_t id =(ptrdiff_t)userData;
    ZMP_ASSERT(id >= 0 && id <= MAX_CAML_INT, printf("%d, %d\n", id, MAX_CAML_INT));

    return zompGetCamlCounterValueInt(id);
}

void statsCreateCamlIntCounter(const char* sectionName, const char* name, int fractionalDigits, int id)
{
    Section* section = statsMainSection()->findNamedChild(sectionName);
    ZMP_ASSERT(section,);

    if(section != NULL)
    {
        statsCreateIntCounter(section, name, fractionalDigits, (void*)id, ReturnCamlCounterValue);
    }
}

static float ReturnCamlCounterValueFloat(Counter*, void* userData)
{
    ptrdiff_t id =(ptrdiff_t)userData;
    ZMP_ASSERT(id >= 0 && id <= MAX_CAML_INT, printf("%d, %d\n", id, MAX_CAML_INT));

    return zompGetCamlCounterValueFloat(id);
}

void statsCreateCamlFloatCounter(const char* sectionName, const char* name, int fractionalDigits, int id)
{
    Section* section = statsMainSection()->findNamedChild(sectionName);
    ZMP_ASSERT(section,);

    if(section != NULL)
    {
        statsCreateFloatCounter(section, name, fractionalDigits, (void*)id, ReturnCamlCounterValueFloat);
    }
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

i32 statsCounterFractionalDigits(Counter* counter)
{
    return counter->fractionalDigits();
}

void statsPrintReport(int indent)
{
    statsPrintReportToStream(stdout, indent);
}

void statsPrintReportToStream(FILE* out, int indent)
{
    fprintf(out, "Zomp statistics\n");
    statsMainSection()->printReport(out, indent);
}

bool statsPrintReportToFile(const char* fileName, int indent)
{
    FILE* file = fopen(fileName, "w");
    if(file == NULL) {
        return false;
    }
    else {
        statsPrintReportToStream(file, indent);
        fclose(file);
        return true;
    }
}


} // extern "C"


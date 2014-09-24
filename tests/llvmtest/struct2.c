
struct foo
{
    int bar, buzz;
    struct foo* next;
};

int main()
{
    struct foo f;
    return f.bar;
}


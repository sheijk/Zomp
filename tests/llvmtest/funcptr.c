

int apply( int (*f)(int), int arg ) {
    return f(arg);
}

int negate(int i) { return -i; }

int main() {
    int (*func)(int) = negate;
    int i = apply(func, 10);

    return 0;
}


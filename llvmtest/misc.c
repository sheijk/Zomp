
void foo(int x) {}

int f = 1;

int add(int l, int r) {
    return l + r;
}

int constant() {
    foo(f);
    
    return 10;
}

int assign() {
    int a = 3;
    int b = 4;
    a = b;
    return 0;
}

int shift(int l, int r) {
    return l << r;
}

int ifthenelse(int t) {
    if( t ) {
        return 1;
    }
    else {
        return 2;
    }
}

int loop() {
    int sum = 0;

    while( sum < 100 ) {
        sum += sum;
    }

    return sum;
}

int compare(int x) {
    return 3 < x;
}


#include <stdlib.h>

struct closure {
    int (*fun)(int x, void *captured);
    void *captured;
};

struct captured {
    int y;
};


int fun(int x, void *cap) {
    int result = x + ((struct captured *)cap)->y;
    return result;
}

int main() {
    struct closure *cl = malloc(sizeof(*cl));
    cl->fun = fun;
    struct captured *cap = malloc(sizeof(*cap));
    cap->y = 1;
    cl->captured = cap;
    cl->fun(1, cl->captured);
    return 0;
}
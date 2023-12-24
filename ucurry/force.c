#include <stdlib.h>
#include <stdbool.h>
#include <stdio.h>

struct closure {
    int (*fun)(char a, void *captured); //TODO: a hard-code one for the closure in simple.uc 
    void *captured;
};

struct thunk {
    void *(*delay_closure); //TODO: since this is a closure we need to 
    void *value;
    bool evaled;
};

// CAUTION: this might lose the closure information! by doing function call like this
void *Force(struct thunk *input) {
    if (input->evaled) {
        return input->value;
    }
    struct closure *cl = (struct closure *)input->delay_closure;
    input->value = (void *) (uintptr_t)cl->fun('u', cl->captured);
    // printf("*(input->value): %d", (int)(uintptr_t)input->value);
    input->evaled = true;
    return input->value;
}

struct thunk *MakeThunk(void *(*delay_function)) {
    struct thunk *ptr = malloc(sizeof(struct thunk));
    ptr->evaled = false;
    ptr->value = NULL;
    ptr->delay_closure = delay_function;
    return ptr;
}

#ifdef BUILD_TEST
int main() {
    return 0;
}
#endif
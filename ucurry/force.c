#include <stdlib.h>
#include <stdbool.h>
#include <stdio.h>

struct thunk {
    void *(*delay_function);
    void *value;
    bool evaled;
};

// CAUTION: this might lose the closure information! by doing function call like this
void *Force(struct thunk *input) {
    if (input->evaled) {
        return input->value;
    }
    input->value = (void *) (uintptr_t)input->delay_function;
    printf("*(input->value): %d", (int)(uintptr_t)input->value);
    input->evaled = true;
    return input->value;
}

struct thunk *MakeThunk(void *(*delay_function)) {
    struct thunk *ptr = malloc(sizeof(struct thunk));
    ptr->evaled = false;
    ptr->value = NULL;
    ptr->delay_function = delay_function;
    return ptr;
}

#ifdef BUILD_TEST
int main() {
    return 0;
}
#endif
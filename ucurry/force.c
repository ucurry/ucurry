#include <stdlib.h>
#include <stdbool.h>

struct thunk {
    void *value;
    void *(*delay_function);
    bool evaled;
};

// CAUTION: this might lose the closure information! by doing function call like this
void *Force(struct thunk *input) {
    if (input->evaled) {
        return input->value;
    }
    input->value = (void *) (uintptr_t)input->delay_function;
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

int main() {
    return 0;
}
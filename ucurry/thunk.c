#include <stdlib.h>
#include <stdbool.h>
#include <stdio.h>

struct thunk {
    bool evaled;
    void *value;
    void *(*function)();
};

int x(){
    return 5;
}

int y(){
    return 6;
}

int minusone(/*cl*/) {
    int x = force(input);
    return x - 1;
}

void *force(struct thunk *input){
    if (input->evaled) {
        return input->value;
    }

    input->value = (void *)(uintptr_t)input->function();
    input->evaled = true;
    return input->value;
}

int add(struct thunk *x, struct thunk *y) {
    int tempx = (int)(uintptr_t)(force(x));
    x - 1;

    int tempy = (int)(uintptr_t)(force(y));
    return tempx + tempy;
}

void caller(){
    struct thunk *xt = malloc(sizeof(struct thunk));
    xt->evaled = false;  
    xt->value = NULL;
    xt->function = x;

    struct thunk *yt = malloc(sizeof(struct thunk));
    yt->evaled = false;  
    yt->value = NULL;
    yt->function = y;

    int z = (int)(uintptr_t)(add(xt, yt));
    printf("%d\n", z);
    return;
}

int fact(struct thunk *n) {
    int x = force(n);
    if (x == 0) {
        return 1;
    } else {
        struct thunk *nx = malloc(sizeof(struct thunk));
        nx->evaled = false;
        nx->value = NULL;
        nx->function = minusone;
        return x * fact();
    }
}



int main(){
    caller();
    return 0;
}
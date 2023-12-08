#include <stdio.h>
#include <stdlib.h>

void nomatch() {
    fprintf(stderr, "Pattern matching is not exhaustive and the case falls through");
    exit(1);
}
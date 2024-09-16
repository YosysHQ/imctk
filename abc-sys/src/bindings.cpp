#include <stdio.h>

#include "bindings.h"

extern "C" {

#include "generated/bindings.c"

void imctk_abc_line_buffer_stdout() {
    setvbuf(stdout, NULL, _IOLBF, 0);
}

}

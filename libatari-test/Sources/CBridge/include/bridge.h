#ifndef BRIDGE_H
#define BRIDGE_H

#include <stdint.h>
#include "libatari800.h"

typedef struct {
    unsigned char byte;
    unsigned short word;
    uint32_t long_val;
} input_t;

typedef struct {
    union {
        uint32_t a;
        unsigned short b;
        unsigned char storage[6];
    };
    unsigned char bytes[16];
} state_t;

void print_hello_world(void);
int return_number(int n);
void read_file_first_line(const char* filename);
void print_input_struct(input_t* input);
state_t* allocate_state_struct(void);
void free_state_struct(state_t* state);

#endif // BRIDGE_H
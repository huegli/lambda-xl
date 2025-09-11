#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include "include/bridge.h"

void print_hello_world(void) {
    printf("Hello World\n");
}

int return_number(int n) {
    return n;
}

void read_file_first_line(const char* filename) {
    FILE* file = fopen(filename, "r");
    if (file == NULL) {
        printf("Error: Could not open file %s\n", filename);
        return;
    }
    
    char line[1024];
    if (fgets(line, sizeof(line), file) != NULL) {
        printf("%s", line);
    }
    
    fclose(file);
}

void print_input_struct(input_t* input) {
    printf("byte: %u, word: %u, long_val: %u\n", input->byte, input->word, input->long_val);
}

state_t* allocate_state_struct(void) {
    state_t* state = (state_t*)malloc(sizeof(state_t));
    if (state == NULL) {
        return NULL;
    }
    
    // Initialize with dummy data
    state->a = 0x12345678;
    
    // Initialize bytes array with some dummy data
    for (int i = 0; i < 16; i++) {
        state->bytes[i] = (unsigned char)(i * 2 + 1);
    }
    
    return state;
}

void free_state_struct(state_t* state) {
    if (state != NULL) {
        free(state);
    }
}
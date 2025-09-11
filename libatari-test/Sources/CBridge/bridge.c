#include "include/bridge.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void print_hello_world(void) {
    printf("Hello World from C!\n");
}

int return_number(int n) {
    return n;
}

void read_file_first_line(const char* filename) {
    FILE* file = fopen(filename, "r");
    if (file) {
        char line[256];
        if (fgets(line, sizeof(line), file)) {
            printf("%s", line);
        }
        fclose(file);
    }
}

void print_input_struct(input_t* input) {
    printf("byte: 0x%02X, word: 0x%04X, long: 0x%08X\n", 
           input->byte, input->word, input->long_val);
}

state_t* allocate_state_struct(void) {
    state_t* state = malloc(sizeof(state_t));
    if (state) {
        state->a = 0xDEADBEEF;
        state->bytes[0] = 0x00;
        state->bytes[1] = 0x11;
        state->bytes[2] = 0x22;
        state->bytes[3] = 0x33;
        state->bytes[4] = 0x44;
        state->bytes[5] = 0x55;
        state->bytes[6] = 0x66;
        state->bytes[7] = 0x77;
        state->bytes[8] = 0x88;
        state->bytes[9] = 0x99;
        state->bytes[10] = 0xAA;
        state->bytes[11] = 0xBB;
        state->bytes[12] = 0xCC;
        state->bytes[13] = 0xDD;
        state->bytes[14] = 0xEE;
        state->bytes[15] = 0xFF;
    }
    return state;
}

void free_state_struct(state_t* state) {
    if (state) {
        free(state);
    }
}

// libatari800 function stubs for testing
int libatari800_init(int argc, char **argv) {
    printf("libatari800_init called with %d arguments\n", argc);
    for (int i = 0; i < argc; i++) {
        printf("  arg[%d]: %s\n", i, argv[i]);
    }
    // For now, just return success
    return 0;
}

const char *libatari800_error_message(void) {
    return "No error (stub implementation)";
}

void libatari800_exit(void) {
    printf("libatari800_exit called\n");
}
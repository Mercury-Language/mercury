// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 2009 Ralph Becket <ralphbecket@gmail.com>
// Copyright (C) 2010 The University of Melbourne.
// Copyright (C) 2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// expanding_array.h
//
// Template definition for expanding arrays. The initial_size argument
// must be at least 1. The storage used can be reclaimed with free().

#ifndef MERCURY_EXPANDING_ARRAY_H
#define MERCURY_EXPANDING_ARRAY_H

#define MR_DEFINE_EXPANDING_ARRAY(array_name, item_type, initial_size)  \
                                                                        \
struct struct_##array_name {                                            \
    unsigned int size;                                                  \
    unsigned int occupants;                                             \
    item_type items[1];                                                 \
};                                                                      \
                                                                        \
typedef struct struct_##array_name array_name;                          \
                                                                        \
static array_name *                                                     \
init_##array_name(void)                                                 \
{                                                                       \
    array_name *a = (array_name *) malloc(                              \
        sizeof(array_name) + (initial_size - 1) * sizeof(item_type));   \
    if (a == NULL) {                                                    \
        fprintf(stderr, "%s:%s: init_%s: call to malloc failed.\n",     \
            __FILE__, __LINE__, #array_name);                           \
        exit(1);                                                        \
    }                                                                   \
    a->size = initial_size;                                             \
    a->occupants = 0;                                                   \
    return a;                                                           \
}                                                                       \
                                                                        \
static unsigned int                                                     \
array_name##_num_occupants(array_name *a)                               \
{                                                                       \
    return a->occupants;                                                \
}                                                                       \
                                                                        \
static array_name *                                                     \
array_name##_append_item(array_name *a, item_type x)                    \
{                                                                       \
    unsigned int sz = a->size;                                          \
    unsigned int occs = a->occupants;                                   \
    if (occs == sz) {                                                   \
        sz += sz;                                                       \
        a = (array_name *) realloc(a,                                   \
            sizeof(array_name) + (sz - 1) * sizeof(item_type));         \
        if (a == NULL) {                                                \
            fprintf(stderr, "%s:%s: %s_append_item: "                   \
                "call to realloc failed.\n",                            \
                __FILE__, __LINE__, #array_name);                       \
            exit(1);                                                    \
        }                                                               \
        a->size = sz;                                                   \
    }                                                                   \
    a->items[occs] = x;                                                 \
    a->occupants = occs + 1;                                            \
    return a;                                                           \
}                                                                       \
                                                                        \
static item_type                                                        \
array_name##_remove_last_item(array_name *a)                            \
{                                                                       \
    item_type x;                                                        \
    x = a->items[a->occupants - 1];                                     \
    a->occupants--;                                                     \
    return x;                                                           \
}                                                                       \
                                                                        \
static item_type                                                        \
array_name##_get_item(array_name *a, unsigned int i)                    \
{                                                                       \
    return a->items[i];                                                 \
}                                                                       \
                                                                        \
static void                                                             \
array_name##_set_item(array_name *a, unsigned int i, item_type x)       \
{                                                                       \
    a->items[i] = x;                                                    \
}                                                                       \

#endif // ! MERCURY_EXPANDING_ARRAY_H

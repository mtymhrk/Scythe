#ifndef INCLUDED_UTF8_H__
#define INCLUDED_UTF8_H__

#include <stdint.h>
#include <stdbool.h>

typedef uint8_t utf8chr_t;
typedef struct {
  utf8chr_t chr[4];
} Utf8Chr;

typedef struct Utf8StringRec Utf8String;

Utf8String *utf8str(const void *src, size_t len);
void utf8str_destruct(Utf8String *str);
Utf8String *utf8str_copy(const Utf8String *src);
Utf8String *utf8str_dup(Utf8String *src);
ssize_t utf8str_length(Utf8String *str);
ssize_t utf8str_bytesize(Utf8String *str);
bool utf8str_is_equal(Utf8String *str1, Utf8String *str2);
Utf8String *utf8str_substr(Utf8String *str, unsigned int pos, size_t len);
Utf8String *utf8str_push(Utf8String *str, const Utf8Chr *c);
Utf8String *utf8str_append(Utf8String *str, const Utf8String *append);
Utf8Chr utf8str_get(Utf8String *str, unsigned int pos);
Utf8String *utf8str_set(Utf8String *str, unsigned int pos, const Utf8Chr *c);
Utf8String *utf8str_fill(Utf8String *str,
                         unsigned int pos, size_t len, const Utf8Chr *c);
int utf8str_find_chr(const Utf8String *str, const Utf8Chr *c);
int utf8str_match(const Utf8String *str, const Utf8String *pat);
ssize_t utf8str_dump(const Utf8String *str, void *buf, size_t size);

#endif /* INCLUDED_UTF8_H__ */

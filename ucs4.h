#ifndef INCLUDE_UCS4_H__
#define INCLUDE_UCS4_H__

#include <stdbool.h>
#include <stdint.h>

typedef uint32_t ucs4chr_t;
typedef uint8_t utf8_t;
typedef struct Ucs4StringRec Ucs4String;

#define UCS4CHR(c) ((ucs4chr_t)(c))

void ucs4str_destruct(Ucs4String *str);
Ucs4String *ucs4str_from_ascii(const char *ascii);
Ucs4String *ucs4str_from_utf8(const utf8_t *utf8);
Ucs4String *ucs4str_copy(const Ucs4String *src);
Ucs4String *ucs4str_dup(Ucs4String *src);
ssize_t ucs4str_length(Ucs4String *str);
bool ucs4str_is_equal(Ucs4String *str1, Ucs4String *str2);
Ucs4String *ucs4str_substr(Ucs4String *str, unsigned int pos, size_t len);
Ucs4String *ucs4str_push(Ucs4String *str, ucs4chr_t c);
Ucs4String *ucs4str_append(Ucs4String *str, const Ucs4String *append);
Ucs4String *ucs4str_append_ascii(Ucs4String *str, const char *append);
Ucs4String *ucs4str_append_utf8(Ucs4String *str, const utf8_t *append);
ucs4chr_t ucs4str_get(const Ucs4String *str, unsigned int pos);
Ucs4String *ucs4str_set(Ucs4String *str, unsigned int pos, ucs4chr_t c);
Ucs4String *ucs4str_fill(Ucs4String *str, unsigned int pos,
                         size_t len, ucs4chr_t c);
int ucs4str_find_chr(const Ucs4String *str, ucs4chr_t c);
int ucs4str_match(const Ucs4String *str, const Ucs4String *pat);
int ucs4str_match_ascii(const Ucs4String *str, const char *ascii_pat);
int ucs4str_match_utf8(const Ucs4String *str, const utf8_t *utf8_pat);
int ucs4str_to_ascii(const Ucs4String *str, char *ascii, size_t size);
int ucs4str_to_utf8(const Ucs4String *str, utf8_t *utf8, size_t size);

#endif /* INCLUDE_UCS4_H__ */

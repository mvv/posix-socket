#ifndef POSIX_SOCKET_MARCROS_H
#define POSIX_SOCKET_MARCROS_H

#ifndef hsc_alignment
# define hsc_alignment(t) \
   printf("%lu", (unsigned long) offsetof (struct { char x__; t (y__); }, y__));
#endif
#define hsc_offsetof(s,f) \
  printf("%lu", (unsigned long) offsetof (s, f));
#define hsc_itype(t) \
  printf("%s%lu", ((t)(-1)) < 0 ? "Int" : "Word", sizeof (t) * 8);

#endif /* POSIX_SOCKET_MARCROS_H */


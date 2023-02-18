#include <stdio.h>

main (argc, argv)
  int argc;
  char *argv[];
{
  if (open(argv[1], 0) != ERROR) abort (1);
}

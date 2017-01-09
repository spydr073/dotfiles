/*
 * Change/query the brightness of LCD screen.
 */

#include <stdio.h>

void usage()
{
  fprintf(stderr, "Usage: lcd-brightness [value]\n");
}

int main(int argc, char *argv[])
{
  FILE *fp;
  int bright = 0;
  const char *kFileName = "/sys/class/backlight/intel_backlight/brightness";

  switch (argc) {
    case 1:
      fp = fopen(kFileName, "r");
      fscanf(fp, "%d", &bright);
      printf("%d\n", bright);
      break;
    case 2:
      fp = fopen(kFileName, "w");
      bright = atoi(argv[1]);
      fprintf(fp, "%d\n", bright);
      break;
    default:
      usage();
      return -1;
  }

  fclose(fp);
  return 0;
}

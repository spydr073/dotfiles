/*
 * Change/query the brightness of LCD screen.
 *
 * Be sure to update the group and permissions for the backlight file.
 *
 *   > sudo chown root:video /sys/class/backlight/intel_backlight/brightness
 *   > sudo chmod 764 /sys/class/backlight/intell_backlight/brightness
 *
 */

#include <stdio.h>
#include <stdlib.h>

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

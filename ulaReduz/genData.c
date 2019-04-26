#include <stdio.h>
#include <stdlib.h>

#define BUF_SZ 32

int buf[BUF_SZ] = {
  0x5555aaaa, 0x5555aaaa, 0x5555aaaa,
  1,4,
  0x5555aaab, 0x5555aaac, 0x5555aaad, 0x55555555, // 55550000
  0x5555aaaa,
  2,3,
  0x5555aaaa, 0x5555aaaa, 0xaaaa5555, // ffffffff
  0x5555aaaa, 0x5555aaaa,
  3,2,
  0x44444444, 0x88888888, // cccccccc
  0x5555aaaa,
  4,2,
  4,4, // 00000010
  0x5555aaaa,
  0,2,
  0x5555aaaa, 0xaaaa5555, // ffffffff
 0x5555aaaa
  // 0x5555aaaa, 0x5555aaaa, 0x5555aaaa, 0x5555aaaa
};


int main(int argc, char *argv[]) {
  int i;
  int number = 4;
  int seed = 7;

  int n;

  for (i=0; i<BUF_SZ; i++) {
    n = buf[i];
    fwrite(&n, sizeof(int), 1, stdout);
  }
  return(0);
}

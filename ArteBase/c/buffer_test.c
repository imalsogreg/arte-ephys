#include <stdio.h>
#include "buffer.h"

bool append_test(void);

int main(int argc, char *argv[]){

  if(append_test){
    return 0;
  } else {
    return 1;
  }

}

bool append_test (void){
  
  struct linear_buffer   myLinBuffer;
  struct circular_buffer myCircBuffer;

  // 2 samples, 3 channels
  in_buffer = setup_linear_buffer( 2, 3, &myLinBuffer );

  for 

}

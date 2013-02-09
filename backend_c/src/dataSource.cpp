#include <yaml.h>
#include "dataSource.h"


DataSource::DataSource () {
  init();
}


void DataSource::init() {
  
#ifdef USE_NIDAQMX
  printf("Using nidaqmx\n");
#else
  printf("Without nidaqmx\n");
#endif

}

int main(int argc, char *argv[]) {

  DataSource *mySource = new DataSource ();

  return 0;
}

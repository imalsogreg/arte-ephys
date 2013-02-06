// dataSource.h
//
// Connect to zmq command port, listen for commands to:
//  - Initialize nidaq cards
//  - Reset timer
//  - Configure self wrt session-config file
//  - Organize data into channels for different tetrodes
//  - Publish that data to workers.  
//      Workers will do the rest (filtering, sending to disk-sink, sending to network)

#ifndef DATASOURCE_H_
#define DATASOURCE_H_

class DataSource {

 public:
  DataSource();

 private:
  void init();

};





#endif

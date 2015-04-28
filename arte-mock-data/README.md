#arte-mock-data

A server to mock out arte-backend. Call arte-mock-data on a .tt file and it will convert this into a UDP pub/sub server producing messages with times determined by the recorded spikes.

(In order to stream position data and mulitple tetrodes' spike data independently from multiple instances of arte-mock-data, we need some way of coordinating them. Favorite idea for this so far is to have each arte-mock-data process connect to a single arte-command process, and have arte-command issue a command like 'reset clocks and start mock data sources')
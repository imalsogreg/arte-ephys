#ifndef SPIKE_H_
#define SPIKE_H_

#include "buffer.h"

struct backend_spike {
  double **samps;
  int n_samps_per_chan;
};

void trode_scan_for_spikes( struct circular_buffer* c_buffer, 
			    struct spikes*, 
			    int n_spikes );

#endif

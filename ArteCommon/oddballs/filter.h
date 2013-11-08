#ifndef FILTER_H_
#define FILTER_H_

#include "buffer.h"

// Filter coefficients for a second-order-section
// Assumes a1 = 1, meaning that sections with 
// a1 different must divide all coefficients through
// by a1 to get into this form. (my b0 = your b0 / your a1)
struct filter_section {
  double b0;
  double b1;
  double b2;
  double a1;
  double a2;
};

// IIR filter a a collection of second-order sections
struct iir_filter {
  int n_section;
  struct *filter_section;
};

// Write circular-buffer contents through filter
// into out_buffer (and update all cursor positions)
void filter_buffer_into( struct circular_buffer *in_buffer,
			 struct circular_buffer **intermediate_buffers,
			 struct iir_filter *filt,
			 int n_samples,
			 struct circular_buffer *out_buffer );


#endif

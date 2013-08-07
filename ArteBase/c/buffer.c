#include "buffer.h"

#include <stdlib.h>

void setup_circular_buffer( int n_samples_per_channel,
			    int n_channels,
			    double dt,
			    struct circular_buffer *new_circular_buffer ){
  int n_samples_total;
  n_bytes = n_namples_per_channel * n_channels * sizeof(double);
  new_circular_buffer->buffer_data = (double *) malloc( n_bytes );
  new_circular_buffer->n_channels = n_channels;
  new_circular_buffer->n_samples_per_channel = n_samples_per_channel;
  new_circular_buffer->first_fresh_sample_curser   = -1;
  new_circular_buffer->last_fresh_sample_cursor    = -1;
  new_circular_buffer->dt                          = dt;
  new_circular_buffer->last_fresh_sample_cursor_t = -1;
}

void cleanup_circular_buffer( struct circular_buffer *buffer ){
  free( buffer->buffer_data );
}

void setup_linear_buffer( int n_samples_per_channel,
			  int n_channels,
			  double dt,
			  struct linear_buffer *new_linear_buffer ){
  int n_samples_total;
  n_bytes = n_samples_per_channel * n_channels * sizeof(double);
  new_linear_buffer->buffer_data = (double *) malloc( n_bytes );
  new_linear_buffer->n_samples_per_channel = n_samples_per_channel;
  new_linear_buffer->n_channels            = n_channels;
  new_linear_buffer->dt                    = dt;
  new_linear_buffer->first_sample_t        = (-1);
}

void cleanup_linear_buffer( struct linear_buffer *buffer){
  free( buffer->buffer_data );
}


void append_data( struct linear_buffer   *in_buffer,
	     struct circular_buffer *c_buffer ){

  int chan;
  int in_samp;
  int out_samp;
  int in_n_samp_pc; // input buffer n samps per chan
  int c_n_samp_pc;  // circular buffer n samps per chan

  in_n_samp_pc = in_buffer->n_samples_per_channel;
  c_n_samp_pc  = c_buffer->n_samples_per_channel;

  for (in_samp = 0; in_samp < c_n_samp_pc; in_samp++){

    out_samp = c_buffer->last_fresh_sample_cursor + 1;
    if( out_samp == c_n_samp ){
      out_samp = 0;
    }

    for (chan = 0; chan < in_buffer->n_chans; chan++ ){
      // relies on sample-major ordering
      c_buffer->buffer_data[out_samp*c_n_samp_pc + chan] =
	in_buffer->buffer_data[in_samp*in_n_samp_pc + chan];
    }

  }

  // Adjust the cursors and timepost
  c_buffer->last_fresh_sample_t = 
    in_buffer->first_sample_t+(dt*(in_n_samp_pc-1));
  c_buffer->last_fresh_sample_cursor =
    (c_buffer->last_fresh_sample_cursor + in_n_samp_pc) % c_n_samp_pc;

}

bool append_shapes_ok( struct linear_buffer   *in_buffer,
		       struct circular_buffer *c_buffer ){
  double epsilon; // equality margin for floats
  epsilon = 1e-9;
  return 
    ((in_buffer->n_samples_per_channel == c_buffer->n_samples_per_channel)
    &&
    (in_buffer->n_channels == c_buffer->n_channels)
    &&
     (abs(in_buffer->dt - c_buffer->dt) < epsilon));
}

void print_linear_buffer( struct linear_buffer *buffer){
  
}

void print_circular_buffer( struct circular_buffer *buffer ){
  // TODO
}

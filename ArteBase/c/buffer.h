#ifndef BUFFER_H_
#define BUFFER_H_

// 2D circular buffer holding voltage data and time (seconds
//   since clock reset
// Sample-major order ( time0_chan0, t0_c1, t0_c2,, t1_c0, t1_c1, t1_c2 )
// Fresh_sample_cursors point to data needing to be processed and should
// always correspond to indices within the data (updating functions mod
//  them back to zero when they go beyond the end of the buffer)
struct circular_buffer {
  double *buffer_data;
  int n_channels;
  int n_samples_per_channel;
  int first_fresh_sample_cursor;
  int last_fresh_sample_cursor;
  double dt;
  double last_fresh_sample_t;
};

// 2D simple buffer holding voltage data and time (seconds) 
//   since clock reset
struct linear_buffer {
  double *buffer_data;
  int n_channels;
  int n_sampels_per_channel;
  double dt;
  double first_sample_t;
};

void setup_circular_buffer( int n_samples_per_channel,
			    int n_channels,
			    double dt,
			    struct circular_buffer *new_cicrular_buffer );

void cleanup_circular_buffer( struct circular_buffer *buffer );

void setup_linear_buffer( int n_samples_per_channel,
			  int n_channels,
			  struct linear_buffer *new_linear_buffer );

void cleanup_linear_buffer( struct linear_buffer *buffer );

// Add sample-major data to sample-major circular buffer
void append_data( struct linear_buffer   *in_buffer, 
		  struct circular_buffer *c_buffer );

// Find out if the formats of the buffer and data are compatible
bool append_shapes_ok( struct linear_buffer   *in_buffer,
		       struct circular_buffer *c_buffer );

void print_circular_buffer( struct circular_buffer *buffer );

// Implement these if they look like they'll be useful

// Add channel-major data to sample-major circular buffer
//void append_chan_major_data ( struct linear_buffer   *in_buffer,
//			      struct circular_buffer *c_buffer );

// Find out if the formats of the buffer and data are compatible
//bool append_shapes_chan_major_ok ( struct linear_buffer *in_buffer_cm,
//				   struct circular_buffer *c_buffer );

#endif

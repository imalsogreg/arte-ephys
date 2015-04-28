#arte-decoder

Train a model P(Spikes | Position) using rat's position and cell spikes from experiment time (0,t - delta), and predict P(Position | Spikes) using the spikes from experiment time (t - delta, t).

The arte-decoder executable should run in one of two modes:

 - *Single-tetrode mode*: Subscribe to a source of spikes from one tetrode. Set up a Req/Rep channel for sending the decoding output to the *Master-decoder mode* process. One *Single-tetrode mode* process will be run in parallel for each tetrode in the recording rig
 - *Master-decoder mode*: Connect to all *Single-tetrode mode* processes by Rep/Req. At a fixed interval (usually ~20ms), request a decoding from all of them simultaneously - collect the results and combine them into a single position estimate.
## Arte Master

### How it works

1. Configuration files live on the 'master' computer, in ~/.arte-ephys/

  * backend.conf: DAQ card & timing configuration
  * session.conf: Tetrode settings, gains, thresholds, DAQ sources, display prefs
  * tracker.conf: Camera device file, threshold level, etc.
  * master.conf : Mapping of processes to machines, port numbers, auto-start options

2. User starts arte-master, creates a GUI.  No other programs are running.

3. arte-master sets up its own gui.  Gui and zmq polling run in parallel.

4. arte-master issues commands via ssh to slave computer, like: arte-backend 5223, in order to start arte-backend.

5. The processes started by arte-master are each servers that start on launch and bind the portgiven as command-line argument.  arte-master starts a client and connects in Rep-Req mode to that process.

6. arte-master sends a configuration object to each child process.  Child process uses this config to set itself up.

7. Part of this configuration is another data-flow diagram for emitting/collecting spikes, waveforms, etc.  This is a parallel thing going on: stream data from the daq cards, finding spikes, sending them to plotter programs, plotting them, etc.

8. The user issues a command (eg - "Reset timers") from arte-master.  arte-master sends this command to the right processes as a Request, and receives a Response.

9. The user of the gui of a child process (like a spike viewer) issues a command to change a spike threshold on a channel.  This command is stored locally in the child program, it doesn't have any effect yet.

10. arte-master periodically (5 Hz or so) pings child processes to see their running state.  Part of the state is: do you have any pending commands?  If so, those commands come back as Response to arte-master.  arte-master interprets the command and issues the appropriate 'real' command as a new Request.

11. Quitting is another master-level command.  A quit is treated by first sending quit commands to all child processes, and collecting the results (does each child say that it's in fact Ok to quit?). 
Installing an ArtE System in a Wilson-Lab recording Room
========================================================

Find buyable hardware
---------------------

- [ ] Backend computer
- [ ] Visualization computer
- [ ] Router & some ethernet cables
- [ ] [Camera](http://www.ptgreystore.com/firefly-mv-03-mp-mono-firewire), [Lens](http://www.ptgreystore.com/fujinon-yv28x28sa-2-hd-vari-focal-lens), and long [cable](http://www.ptgreystore.com/products/212-45-meter-6-pin-to-6-pin-ieee-1394a-cable.aspx)
- [ ] PCI firewire [adapter card](http://www.ptgreystore.com/ieee-1394a-ohci-pci-host-adapter-3-port-400-mbps-card)

* Backend computer must be fast
** Two to four PCI slots
** Intel i7 processor
** Link to tested motherboards, NI cards

* Vis computer is less strict, just needs a fast processor, nVidia card (need link), and a PCI port.

* We might try some [System76 machines](http://www.system76.com) for this.  Do any meet the specs?



Hardware to build
-----------------

- [ ] Now - Either get old amps from Rat1 and build another of Stuart's patch-boxes

- [ ] Now - or make our own inamp boards

- [ ] Future - Move old PCB123 Preamp chip to Eagle

- [ ] Future - Add ground plane to preamp

- [ ] Future - Add connector for shared power

- [ ] Future -  Add proper mounts for always-on LED's

- [ ] Future - Move old PCB123 EIB to Eagle, add two TT's and a ground plane

- [ ] Future - Buy opamp chips, Hirose connectors, LED's for the preamps, assemble.

- [ ] Future - 32-tt finewire

- [ ] Future - Update Stuart's patch-box for 32 TT's.

- [ ] Preamp test chip


Computer Setup
--------------

- [ ] Install CentOS 5 on backend computer

- [ ] Setup ethernet teathering through vis computer

- [ ] Build arte-ephys.googlecode.com code on backend

- [ ] Build arte-ephys.googlecode.com vis. code on vis computer

- [ ] Build arte-ephys github code on vis computer and backend,


Unfinshed necessary features in new arte
----------------------------------------

- [ ] Forward old-arte network spikes to arte

- [ ] forward old-arte network eeg to arte

- [ ] Move to new common clock

- [ ] Finish old arte tracker enough to get by

- [ ] Make a rough spike-viewer for adjusting at least


Documentation
-------------

- [ ] Take very thorough notes at each step.  Goal is to have a presentable instruction manual (that gets the history of the development / changes, and looks presentable and easy to follow when everything stabilizes)
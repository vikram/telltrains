#!/bin/sh

rm /Users/vb/logs/remote-openmcl-dribble    2>/dev/null
rm /Users/vb/logs/remote-openmcl-tty.log    2>/dev/null
rm /Users/vb/logs/remote-openmcl.pid        2>/dev/null
rm /Users/vb/logs/remote-openmcl-socket     2>/dev/null

/usr/local/bin/detachtty --dribble-file /Users/vb/logs/remote-openmcl-dribble \
                   --log-file /Users/vb/logs/remote-openmcl-tty.log \
                   --pid-file /Users/vb/logs/remote-openmcl.pid \
                   /Users/vb/logs/remote-openmcl-socket \
                   /Users/vb/openmcl


#!/bin/sh

rm /home/vb/logs/remote-sbcl-dribble    2>/dev/null
rm /home/vb/logs/remote-sbcl-tty.log    2>/dev/null
rm /home/vb/logs/remote-sbcl.pid        2>/dev/null
rm /home/vb/logs/remote-sbcl-socket     2>/dev/null

/usr/bin/detachtty --dribble-file /home/vb/logs/remote-sbcl-dribble \
                   --log-file /home/vb/logs/remote-sbcl-tty.log \
                   --pid-file /home/vb/logs/remote-sbcl.pid \
                   /home/vb/logs/remote-sbcl-socket \
                   /usr/local/bin/sbcl


#!/bin/sh

rm /home/vb/logs/remote-sbcl-dribble2    2>/dev/null
rm /home/vb/logs/remote-sbcl-tty2.log    2>/dev/null
rm /home/vb/logs/remote-sbcl2.pid        2>/dev/null
rm /home/vb/logs/remote-sbcl-socket2     2>/dev/null

/usr/bin/detachtty --dribble-file /home/vb/logs/remote-sbcl-dribble2 \
                   --log-file /home/vb/logs/remote-sbcl-tty2.log \
                   --pid-file /home/vb/logs/remote-sbcl2.pid \
                   /home/vb/logs/remote-sbcl-socket2 \
                   /usr/local/bin/sbcl --dynamic-space-size 2048


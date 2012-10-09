#!/bin/sh

rm /home/vb/logs/remote-sbcl-dribble1    2>/dev/null
rm /home/vb/logs/remote-sbcl-tty1.log    2>/dev/null
rm /home/vb/logs/remote-sbcl1.pid        2>/dev/null
rm /home/vb/logs/remote-sbcl-socket1     2>/dev/null

/usr/bin/detachtty --dribble-file /home/vb/logs/remote-sbcl-dribble1 \
                   --log-file /home/vb/logs/remote-sbcl-tty1.log \
                   --pid-file /home/vb/logs/remote-sbcl1.pid \
                   /home/vb/logs/remote-sbcl-socket1 \
                   /usr/local/bin/sbcl --dynamic-space-size 2048


grep pound /var/log/syslog.0 | cut -f 7 -d ' ' | sort | uniq -c | less

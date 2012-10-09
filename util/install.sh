#!/bin/sh
# virtualmin-install.sh
# Copyright 2005-2007 Virtualmin, Inc.
# Simple script to grab the virtualmin-release and virtualmin-base packages.
# The packages do most of the hard work, so this script can be small-ish and 
# lazy-ish.

# WARNING: Anything not listed in the currently supported systems list is not
# going to work, despite the fact that you might see code that detects your
# OS and acts on it.  If it isn't in the list, the code is not complete and
# will not work.  More importantly, the packages that this script installs
# are not complete, if the OS isn't listed.  Don't even bother trying it.
#
# A manual install might work for you though.
# See here: http://www.virtualmin.com/support/documentation/virtualmin-admin-guide/ch02.html#id2654070

# Currently supported systems:
prosupported=" Fedora Core 3-6 on i386 and x86_64
 CentOS and RHEL 3-5 on i386 and x86_64
 OpenSUSE 10.0 on i586 and x86_64
 Mandriva 2007 on i386
 Debian 3.1 and 4.0 on i386 and amd64
 Ubuntu 6.06 LTS on i386 and amd64"
gplsupported=" CentOS 5 on i386 and x86_64
 Debian 4.0 on i386 and amd64"

LANG=
export LANG

case $1 in
	--help|-h)
		echo "Usage: `basename $0` [--uninstall|-u|--help|-h]"
		echo "  If called without arguments, installs Virtualmin Professional."
		echo
		echo "  --uninstall|-u: Removes all Virtualmin packages (do not use on production systems)"
		echo "  --help|-h: This message"
		echo
		exit 0
	;;
	--uninstall|-u)
		mode="uninstall"
	;;
	*)
	;;
esac

SERIAL=5555676
KEY=AEWJCAXZMIA
VER=EA4.2
arch=`uname -m`
if [ "$arch" = "i686" ]; then
	arch=i386
fi
if [ "$SERIAL" = "GPL" ]; then
	LOGIN=""
	PRODUCT="GPL"
	supported=$gplsupported
  repopath="gpl/"
else
	LOGIN="$SERIAL:$KEY@"
	PRODUCT="Professional"
	supported=$prosupported
  repopath=""
fi

# Virtualmin-provided packages
vmpackages="usermin webmin wbm-virtualmin-awstats wbm-virtualmin-dav wbm-virtualmin-dav wbm-virtualmin-htpasswd wbm-virtualmin-svn wbm-virtual-server ust-virtual-server-theme wbt-virtual-server-theme"
deps=
# Red Hat-based systems 
rhdeps="bind bind-utils caching-nameserver httpd postfix bind spamassassin procmail perl-DBD-Pg perl-DBD-MySQL quota iptables openssl python mailman subversion ruby rdoc ri mysql mysql-server postgresql postgresql-server rh-postgresql rh-postgresql-server logrotate webalizer php php-domxl php-gd php-imap php-mysql php-odbc php-pear php-pgsql php-snmp php-xmlrpc php-mbstring mod_perl mod_python cyrus-sasl dovecot spamassassin mod_dav_svn cyrus-sasl-gssapi mod_ssl ruby rubygems perl-XML-Simple perl-Crypt-SSLeay"
# SUSE yast installer systems (SUSE 9.3 and OpenSUSE 10.0)
yastdeps="webmin usermin postfix bind perl-spamassassin spamassassin procmail perl-DBI perl-DBD-Pg perl-DBD-mysql quota openssl mailman subversion ruby mysql mysql-Max mysql-administrator mysql-client mysql-shared postgresql postgresql-pl postgresql-libs postgresql-server webalizer apache2 apache2-devel apache2-mod_perl apache2-mod_python apache2-mod_php4 apache2-mod_ruby apache2-worker apache2-prefork clamav awstats dovecot cyrus-sasl cyrus-sasl-gssapi proftpd php4 php4-domxml php4-gd php4-imap php4-mysql php4-mbstring php4-pgsql php4-pear php4-session"
# SUSE rug installer systems (OpenSUSE 10.1)
rugdeps="webmin usermin postfix bind perl-spamassassin spamassassin procmail perl-DBI perl-DBD-Pg perl-DBD-mysql quota openssl mailman subversion ruby mysql mysql-Max mysql-administrator mysql-client mysql-shared postgresql postgresql-pl postgresql-libs postgresql-server webalizer apache2 apache2-devel apache2-mod_fcgid apache2-mod_perl apache2-mod_python apache2-mod_php5 apache2-mod_ruby apache2-worker apache2-prefork clamav clamav-db awstats dovecot cyrus-sasl cyrus-sasl-gssapi proftpd php5 php5-domxml php5-gd php5-imap php5-mysql php5-mbstring php5-pgsql php5-pear php5-session"
# Mandrake/Mandriva
urpmideps="apache2 apache2-common apache2-manual apache2-metuxmpm apache2-mod_dav apache2-mod_ldap apache2-mod_perl apache2-mod_php apache2-mod_proxy apache2-mod_suexec apache2-mod_ssl apache2-modules apache2-peruser apache2-worker clamav clamav-db clamd bind bind-utils caching-nameserver cyrus-sasl postfix postfix-ldap postgresql postgresql-contrib postgresql-docs postgresql-pl postgresql-plperl postgresql-server proftpd proftpd-anonymous quota perl-Net_SSLeay perl-DBI perl-DBD-Pg perl-DBD-mysql spamassassin perl-Mail-SpamAssassin mailman subversion subversion-server MySQL MySQL-common MySQL-client openssl ruby usermin webmin webalizer awstats dovecot perl-XML-Simple perl-Crypt-SSLeay"
# Debian
debdeps="postfix postfix-tls postfix-pcre webmin usermin ruby libapache2-mod-ruby libxml-simple-perl libcrypt-ssleay-perl unzip zip"
# Ubuntu (uses odd virtual packaging for some packages that are separate on Debian!)
ubudeps="postfix postfix-pcre webmin usermin ruby libapache2-mod-ruby libxml-simple-perl libcrypt-ssleay-perl unzip zip"
# Ports-based systems (FreeBSD, NetBSD, OpenBSD)
portsdeps="postfix bind9 p5-Mail-SpamAssassin procmail perl p5-Class-DBI-Pg p5-Class-DBI-mysql setquota openssl python mailman subversion ruby irb rdoc ri mysql-client mysql-server postgresql-client postgresql-server postgresql-contrib logrotate awstats webalizer php4 clamav dovecot cyrus-sasl"
# Gentoo
portagedeps="postfix bind spamassassin procmail perl DBD-Pg DBD-mysql quota openssl python mailman subversion ruby irb rdoc mysql postgresql logrotate awstats webalizer php Net-SSLeay iptables clamav dovecot"

yesno () {
	while read line; do
		case $line in
			y|Y|Yes|YES|yes|yES|yEs|YeS|yeS) return 0
			;;
			n|N|No|NO|no|nO) return 1
			;;
			*)
			printf "\nPlease enter y or n: "
			;;
		esac
	done
}

# Perform an action, log it, and run the spinner throughout
runner () {
	msg=$1
	cmd=$2
	touch busy
	logger_info "$msg"
	$srcdir/spinner busy &
	if $cmd >> /root/virtualmin-install.log; then
		rm busy
		sleep 1
		success "$msg"
		return 0
	else
		rm busy
		sleep 1
		echo "$msg failed.  Error (if any): $?"
		echo
		echo "Displaying the last 15 lines of the install.log to help troubleshoot this problem:"
		tail -15 /root/virtualmin-install.log
		return 1
	fi
}

fatal () {
	echo
	logger_fatal "Fatal Error Occurred: $1"
	logger_fatal "Cannot continue installation."
	logger_fatal "Attempting to remove virtualmin repository configuration, so the installation can be "
	logger_fatal "re-attempted after any problems have been resolved."
	remove_virtualmin_release
	if [ -x $tempdir ]; then
		logger_fatal "Removing temporary directory and files."
		rm -rf /tmp/.virtualmin*
	fi
	logger_fatal "If you are unsure of what went wrong, you may wish to review the log"
	logger_fatal "in /root/virtualmin-install.log."
	exit
}

remove_virtualmin_release () {
	case $os_type in
		"fedora" | "centos" |	"rhel"	)	rpm -e virtualmin-release
		;;
		"suse"	)
			vmsrcs=`y2pmsh source -s | grep "virtualmin" | grep "^[[:digit:]]" | cut -d ":" -f 1`
			y2pmsh source -R $vmsrcs
		;;
		"mandriva" )
			rpm --import http://software.virtualmin.com/lib/RPM-GPG-KEY-virtualmin
			rpm --import http://software.virtualmin.com/lib/RPM-GPG-KEY-webmin
			urpmi.removemedia virtualmin
			urpmi.removemedia virtualmin-universal
			rpm -e virtualmin-release
      	;;
		"debian" | "ubuntu" )
			grep -v "virtualmin" /etc/apt/sources.list > /tmp/sources.list
			mv /tmp/sources.list /etc/apt/sources.list 
		;;
	esac
}

detect_ip () {
	primaryaddr=`/sbin/ifconfig eth0|grep 'inet addr'|cut -d: -f2|cut -d" " -f1`
	# XXX syntax is probably wrong for this test 
	if [ $primaryaddr ]; then
		logger_info "Primary address detected as $primaryaddr"
		address=$primaryaddr
		return 0
	else
		logger_info "Unable to determine IP address of primary interface."
		echo "Please enter the name of your primary network interface: "
		read primaryinterface
		primaryaddr=`/sbin/ifconfig $primaryinterface|grep 'inet addr'|cut -d: -f2|cut -d" " -f1`
		if [ $primaryaddr ]; then
			logger_info "Primary address detected as $primaryaddr"
			address=$primaryaddr
		else
			fatal "Unable to determine IP address of selected interface.  Cannot continue."
		fi
		return 0
	fi
}

set_hostname () {
	i=0
	while [ $i -eq 0 ]; do
		printf "Please enter a fully qualified hostname (for example, virtualmin.com): "
		read line
		if ! is_fully_qualified $line; then
			logger_info "Hostname $line is not fully qualified."
		else
			hostname $line
			detect_ip
			if grep $address /etc/hosts; then
				logger_info "Entry for IP $address exists in /etc/hosts."
				logger_info "Updating with new hostname."
				shortname=`echo $line | cut -d"." -f1`
				sed -i "s/^$address\([\s\t]+\).*$/$address\1$line\t$shortname/" /etc/hosts
			else
			logger_info "Adding new entry for hostname $line on $address to /etc/hosts."
			echo -e "$address\t$line\t$shortname" >> /etc/hosts
		fi
		i=1
	fi
	done
}
  
is_fully_qualified () {
	case $1 in
		localhost.localdomain)
			logger_info "Hostname cannot be localhost.localdomain."
			return 1
		;;
		*.*)
			logger_info "Hostname OK: fully qualified as $1"
			return 0
		;;
	esac
	logger_info "Hostname $name is not fully qualified."
	return 1
}

success () {
	logger_info "$1 Succeeded."
}

# This function performs a rough uninstallation of Virtualmin
# It is neither complete, nor correct, but it almost certainly won't break
# anything.  It is primarily useful for cleaning up a botched install, so you
# can run the installer again.
uninstall () {
	# This is a crummy way to detect package manager...but going through 
    # half the installer just to get here is even crummier.
	if which rpm>/dev/null; then package_type=rpm
	elif which dpkg>/dev/null; then package_type=deb
	fi

	case $package_type in
		rpm)
			rpm -e --nodeps virtualmin-base
			rpm -e --nodeps wbm-virtual-server wbm-virtualmin-htpasswd wbm-virtualmin-dav wbm-virtualmin-mailman wbm-virtualmin-awstats wbm-virtualmin-svn
			rpm -e --nodeps wbt-virtual-server-theme ust-virtual-server-theme
			rpm -e --nodeps webmin usermin awstats
		;;
		deb)
			dpkg --purge virtualmin-base
			dpkg --purge webmin-virtual-server webmin-virtualmin-htpasswd webmin-virtualmin-dav webmin-virtualmin-mailman webmin-virtualmin-awstats webmin-virtualmin-svn
			dpkg --purge webmin-virtual-server-theme usermin-virtual-server-theme
			dpkg --purge webmin usermin webmin-*
			apt-get clean
		;;
		*)
			echo "I don't know how to uninstall on this operating system."
		;;
	esac
	remove_virtualmin_release
	echo "Done.  There's probably quite a bit of related packages and such left behind..."
	echo "but all of the Virtualmin-specific packages have been removed."
	exit 0
}

# XXX Needs to move after os_detection
if [ "$mode" = "uninstall" ]; then
	uninstall
fi

cat <<EOF

Welcome to the Virtualmin $PRODUCT installer, version $VER

 WARNING: This is an Early Adopter release.

 The installation is quite stable and functional when run on a freshly
 installed supported Operating System, but upgrades from existing .wbm-
 based systems, or systems that already have Apache VirtualHost 
 directives or mail users, will very likely run into numerous problems.
 Please read the Virtualmin and Early Adopter FAQs before proceeding if
 your system is not a freshly installed and supported OS.

 This script is not intended to update your system.  It should only be
 used to install Virtualmin Professional, or to upgrade a Virtualmin
 GPL system that was installed manually via .wbm packages.  If you have
 previously installed Virtualmin via this script or the GPL version of 
 this script, upgrades and updates should be handled within Virtualmin
 only.

 The systems currently supported by our install.sh are:
EOF
echo "$supported"
cat <<EOF

 If your OS is not listed above, this script will fail and attempting
 to run it on an unsupported OS is not recommended, or supported.  The
 recommended operating systems for Virtualmin Professional, and more
 details about them, can be found here: 

   http://www.virtualmin.com/os-support.html
 
EOF
	printf " Continue? (y/n) "
	if ! yesno
	then exit
	fi

get_mode () {
cat <<EOF
 FULL or MINIMAL INSTALLATION
 It is possible to install only the minimum set of components and 
 and perform no configuration changes to existing mail/web/DNS 
 or packages.  This mode of installation is called the minimal mode
 because only Webmin, Usermin and the Virtualmin-related modules and
 themes are installed.  The minimal mode will not modify your
 existing configuration.  The full install is recommended if
 this system is a fresh install of the OS.  If your system has
 a working Virtualmin GPL installation using components other than
 our defaults, or you already have virtual hosts, users, mailboxes, 
 etc. configured manually or with another administration tool, the 
 minimal mode is a much safer choice.

EOF

	printf " Would you like to perform a full installation? (y/n) "
	if yesno; then mode=full
	else mode=minimal
	fi

	echo "Installation type: $mode"
	sleep 3
}

# Set the mode (switch to get_mode when 
# minimal mode is finished)
#get_mode
mode=full

virtualminmeta="virtualmin-base"
# If minimal, we don't install any extra packages, or perform any configuration
if [ "$mode" = "minimal" ]; then
	rhdeps=yastdeps=debdeps=ubudeps=portagedeps=portsdeps=""
	virtualminmeta=$vmpackages
fi

# Check for wget or curl
printf "Checking for curl or wget..."
if [ -x "/usr/bin/curl" ]; then
	download="/usr/bin/curl -s -O "
elif [ -x "/usr/bin/wget" ]; then
	download="/usr/bin/wget -nv"
else
	echo "No web download program available: Please install curl or wget"
	echo "and try again."
	exit 1
fi
printf "found $download\n"

# download()
# Use $download to download the provided filename or exit with an error.
download() {
	if $download $1
	then
		success "Download of $1"
    	return $?
	else
		fatal "Failed to download $1."
	fi
}

# Checking for perl
printf "Checking for perl..."
if [ -x "/usr/bin/perl" ]; then
	perl="/usr/bin/perl"
elif [ -x "/usr/local/bin/perl" ]; then
	perl="/usr/local/bin/perl"
else
	echo "Perl was not found on your system: Please install perl and try again"
	exit 1
fi
printf "found $perl\n"

# Only root can run this
id | grep "uid=0(" >/dev/null
if [ "$?" != "0" ]; then
	uname -a | grep -i CYGWIN >/dev/null
	if [ "$?" != "0" ]; then
		echo "Fatal Error: The Virtualmin install script must be run as root"
		twolines
		exit 1
	fi
fi

# Find temp directory
if [ "$tempdir" = "" ]; then
	tempdir=/tmp/.virtualmin-$$
	if [ -e "/tmp/.virtualmin*" ]; then
		rm -rf /tmp/.virtualmin*
	fi
	mkdir $tempdir
fi

# "files" subdir for libs
mkdir $tempdir/files
srcdir=$tempdir/files
cd $srcdir

# Download spinner
$download http://software.virtualmin.com/lib/spinner
chmod +x spinner

# Setup log4sh so we can start keeping a proper log while also feeding output
# to the console.
echo "Loading log4sh logging library..."
if $download http://software.virtualmin.com/lib/log4sh
then 
	# source log4sh (disabling properties file warning)
	LOG4SH_CONFIGURATION="none" . ./log4sh
else
	echo " Could not load logging library from software.virtualmin.com.  Cannot continue."
	echo " We're not just stopping because we don't have a logging library--this probably"
	echo " indicates a serious that will prevent successful installation anyway."
	echo " Check network connectivity, name resolution and disk space and try again."
	exit 1
fi

# Setup log4sh properties
# Console output
logger_setLevel INFO
# Debug log
logger_addAppender virtualmin
appender_setAppenderType virtualmin FileAppender
appender_setAppenderFile virtualmin /root/virtualmin-install.log
appender_setLevel virtualmin ALL
appender_setLayout virtualmin PatternLayout
appender_setPattern virtualmin '%p - %d - %m%n'

logger_info "Started installation log in virtualmin-install.log"

# Print out some details that we gather before logging existed
logger_debug "Install mode: $mode"
logger_debug "Virtualmin Meta-Package list: $virtualminmeta"
logger_debug "install.sh version: $VER"

# Check for a fully qualified hostname
logger_info "Checking for fully qualified hostname..."
name=`hostname -f`
if ! is_fully_qualified $name; then set_hostname
fi

# Insert the serial number and password into /etc/virtualmin-license
logger_info "Installing serial number and license key into /etc/virtualmin-license"
echo "SerialNumber=$SERIAL" > /etc/virtualmin-license
echo "LicenseKey=$KEY"	>> /etc/virtualmin-license
chmod 700 /etc/virtualmin-license

# Detecting the OS
# Grab the Webmin oschooser.pl script
logger_info "Loading OS selection library..."
download http://software.virtualmin.com/lib/oschooser.pl
logger_info "Loading OS list..."
download http://software.virtualmin.com/lib/os_list.txt

cd ..

# Get operating system type
if [ "$os_type" = "" ]; then
	if [ "$autoos" = "" ]; then
		autoos=1
	fi
	$perl "$srcdir/oschooser.pl" "$srcdir/os_list.txt" $tempdir/$$.os $autoos
	if [ "$?" != 0 ]; then
		exit $?
	fi
	. $tempdir/$$.os
	rm -f $tempdir/$$.os
fi
logger_info "Operating system name:    $real_os_type"
logger_info "Operating system version: $real_os_version"

install_virtualmin_release () {
	# Grab virtualmin-release from the server
	logger_info "Installing virtualmin-release package for $real_os_type $real_os_version..."
	case $os_type in
		fedora)
			if [ -x /usr/sbin/setenforce ]; then
				logger_info "Disabling SELinux during installation..."
				if /usr/sbin/setenforce 0; then logger_debug " setenforce 0 succeeded"
				else logger_info "  setenforce 0 failed: $?"
				fi 
			fi
			package_type="rpm"
			deps=$rhdeps
			install="/usr/bin/yum -y -d 2 install"
			download http://${LOGIN}software.virtualmin.com/${repopath}$os_type/$os_version/$arch/virtualmin-release-latest.noarch.rpm
			if rpm -U virtualmin-release-latest.noarch.rpm; then success
			else fatal "Installation of virtualmin-release failed: $?"
			fi
			;;
		rhel)
			if [ -x /usr/sbin/setenforce ]; then
				logger_info "Disabling SELinux during installation..."
				if /usr/sbin/setenforce 0; then logger_debug " setenforce 0 succeeded"
				else logger_info "  setenforce 0 failed: $?"
				fi
			fi
			package_type="rpm"
			deps=$rhdeps
			if [ -x /usr/bin/up2date ]; then
				install="/usr/bin/up2date --nox"
				echo;echo
				echo "If you haven't run up2date before this installation, the installation"
				echo "will fail.  Have you run up2date at least once before starting this installer?"
				if ! yesno; then
					echo
					echo "Exiting.  Please run 'up2date -u' and then run install.sh again."
					exit
				fi
			else
				# CentOS doesn't always have up2date?
				install="/usr/bin/yum -y -d 2 install"
			fi
			rpm --import /usr/share/rhn/RPM-GPG-KEY
			if [ ! -x /usr/bin/yum ]; then
				# Install yum, which makes installing and upgrading our packages easier
				download http://${LOGIN}software.virtualmin.com/${repopath}$os_type/$os_version/$arch/yum-latest.noarch.rpm
				logger_info "yum not found, installing yum from software.virtualmin.com..."
				if rpm -U yum-latest.noarch.rpm; then success
				else fatal "Installation of yum failed: $?"
				fi
			fi
			download http://${LOGIN}software.virtualmin.com/${repopath}$os_type/$os_version/$arch/virtualmin-release-latest.noarch.rpm
			if rpm -U virtualmin-release-latest.noarch.rpm; then success
			else fatal "Installation of virtualmin-release failed: $?"
			fi
		;;
		suse)
			# No release for suse.  Their RPM locks when we try to import keys...
			package_type="rpm"
			# SUSE uses i586 for x86 binary RPMs instead of i386, but uname -i reports i386
			if [ "$arch" = "i386" ]
			then cputype="i586"
			else cputype="x86_64"
			fi
			case $os_version in
				9.3|10.0)
					deps=$yastdeps
					install="/sbin/yast -i"
					if ! yast -i y2pmsh; then
						fatal "Failed to install y2pmsh package.  Cannot continue."
					fi
					if ! y2pmsh source -a http://${LOGIN}software.virtualmin.com/${repopath}$os_type/$os_version/$cputype; then
						fatal "Unable to add yast2 installation source."
					fi
					if ! y2pmsh source -a http://${LOGIN}software.virtualmin.com/${repopath}universal; then
					fatal "Unable to add yast2 installation source: $?"
					fi
				;;
				10.1|10.2)
					deps=$rugdeps
					install="/usr/bin/rug in -y"
					if ! rug ping; then
						logger_info "The ZENworks Management daemon is not running.  Attempting to start."
						/usr/sbin/rczmd start
						if ! rug ping; then
							fatal "ZMD failed to start, installation cannot continue without functioning package management."
						fi
					fi
					if ! rug sa --type=YUM http://${LOGIN}software.virtualmin.com/${repopath}$os_type/$os_version/$cputype virtualmin; then
						fatal "Unable to add rug installation source: $?"
					fi
					if ! rug sa --type=YUM http://${LOGIN}software.virtualmin.com/${repopath}universal virtualmin-universal; then
						fatal "Unable to add rug installation source: $?"
					fi
				;;
			esac
		;;
		mandriva)
			# No release for mandriva either...
			package_type="rpm"
			deps=$urpmideps
			install="/usr/sbin/urpmi"
			logger_info "Updating urpmi repository data..."
			if urpmi.update -a; then success
			else fatal "urpmi.update failed with $?.  This installation script requires a functional urpmi"
			fi
			# Mandriva uses i586 for x86 binary RPMs instead of i386--uname is also utterly broken
			if [[ "$arch" = "i386" || "$arch" = "unknown" ]]
			then cputype="i586"
			else cputype="x86_64"
			fi
			logger_info "Adding virtualmin-universal repository..."
			if urpmi.addmedia virtualmin-universal http://${LOGIN}software.virtualmin.com/${repopath}universal; then
			success "Adding repository"
			else fatal "Failed to add urpmi source for virtualmin-universal.  Cannot continue."
			fi
			logger_info "Adding virtualmin Mandriva $os_version repository..."
			if urpmi.addmedia virtualmin http://${LOGIN}software.virtualmin.com/${repopath}$os_type/$os_version/$cputype; then
				success
			else fatal "Failed to add urpmi source for virtualmin.  Cannot continue."
			fi
			# Install some keys
			download "http://${LOGIN}software.virtualmin.com/${repopath}$os_type/$os_version/$cputype/virtualmin-release-latest.noarch.rpm"
			if rpm -Uvh virtualmin-release-latest.noarch.rpm; then success
			else fatal "Failed to install virtualmin-release package."
			fi
			rpm --import /etc/RPM-GPG-KEYS/RPM-GPG-KEY-webmin
			rpm --import /etc/RPM-GPG-KEYS/RPM-GPG-KEY-virtualmin
		;;
		freebsd)
			package_type="tar"
			deps=$portsdeps
			download "http://${LOGIN}software.virtualmin.com/${repopath}$os_type/$arch/virtualmin-release-latest.tar.gz"
		;;
		gentoo)
			package_type="ebuild"
			deps=$portagedeps
			install="/usr/bin/emerge"
 			download "http://${LOGIN}software.virtualmin.com/${repopath}$os_type/$arch/virtualmin-release-latest.tar.gz"
 		;;
		debian | ubuntu)
			package_type="deb"
			if [ $os_type = "ubuntu" ]; then
				deps=$ubudeps
				repo="virtualmin-dapper"
			else
				deps=$debdeps
				case $os_version in
					3.1)
						repo="virtualmin-sarge"
					;;
					4.0)
						repo="virtualmin-etch"
					;;
				esac
			fi
			# Make sure universe repos are available
			logger_info "Enabling universe repositories, if not already available..."
			sed -ie "s/#*[ ]*deb \(.*\) universe$/deb \1 universe/" /etc/apt/sources.list
			apt-get update
			install="/usr/bin/apt-get --config-file apt.conf.noninteractive -y --force-yes install"
			export DEBIAN_FRONTEND=noninteractive
			logger_info "Cleaning up apt headers and packages, so we can start fresh..."
			logger_info `apt-get clean`
			# Get the noninteractive apt-get configuration file (this is 
			# stupid... -y ought to do all of this).
			download "http://software.virtualmin.com/lib/apt.conf.noninteractive"
			# Make sure universe is available, and all CD repos are disabled
			logger_info "Disabling any CD-based apt repositories so the process can run without assistance"
			sed -i "s/\(deb[[:space:]]file.*\)/#\1/" /etc/apt/sources.list
			echo "deb http://${LOGIN}software.virtualmin.com/${repopath}$os_type/ $repo main" >> /etc/apt/sources.list
			# Install our keys
			logger_info "Installing Webmin and Virtualmin package signing keys..."
			download "http://software.virtualmin.com/lib/RPM-GPG-KEY-virtualmin"
			download "http://software.virtualmin.com/lib/RPM-GPG-KEY-webmin"
			logger_info `apt-key add RPM-GPG-KEY-virtualmin`
			logger_info `apt-key add RPM-GPG-KEY-webmin`
			logger_info `apt-get update`
			logger_info "Removing Debian standard Webmin package, if they exist (because they're broken)..."
			logger_info "Removing Debian apache packages..."
			logger_debug `apt-get -y --purge remove webmin-core apache apache2`
		;;
		*)
			logger_info " Your OS is not currently supported by this installer."
			logger_info " You may be able to run Virtualmin Professional on your system, anyway,"
			logger_info " but you'll have to install it using the manual installation process."
			logger_info " Refer to Chapter 2 of the Virtualmin Administrator's Guide for more"
			logger_info " information.  You may also wish to open a customer support issue so"
			logger_info " that we can guide you through the process--depending on your needs"
			logger_info " and environment, it can be rather complex."
			logger_info ""
			logger_info " Attempting to trick this automatic installation script into running"
			logger_info " is almost certainly a really bad idea.  Platform support requires"
			logger_info " numerous custom binary executables.  Those packages will almost "
			logger_info " certainly fail to run, or worse, on any platform other than the one"
			logger_info " they were built for."
			exit 1
		;;
	esac

	return 0
}

# Install Functions
install_with_apt () {
	logger_info "Installing Virtualmin and all related packages now using the command:"
	logger_info "$install $virtualminmeta"

	if runner "...in progress, please wait..." "$install $virtualminmeta"; then
		logger_info "Installation of $virtualminmeta completed."
	else
		fatal "Installation failed: $?"
	fi

	logger_info "If you are not regularly updating your system nightly using apt-get"
	logger_info "we strongly recommend you update now, using the following commands:"
	logger_info "apt-get update"
	logger_info "apt-get upgrade"
}

install_with_yum () {
	logger_info "Installing Virtualmin and all related packages now using the command:"
	logger_info "yum -y -d 2 install $virtualminmeta"

	if runner "...in progress, please wait..." "yum -y -d 2 install $virtualminmeta"; then
		logger_info "Installation of $virtualminmeta completed."
	else
		fatal "Installation failed: $?"
	fi

	logger_info "If you are not regularly updating your system nightly using yum or up2date"
	logger_info "we strongly recommend you update now, using the following commands:"
	logger_info "Fedora/CentOS: yum update"
	logger_info "RHEL: up2date -u"
	return 0
}

install_with_yast () {
	logger_info "Installing Virtualmin and all related packages now using the command:"
	logger_info "$install $virtualminmeta"
	sources=`y2pmsh source -s | grep "^[[:digit:]]" | cut -d ":" -f 1`
#	if [ "$sources" != "" ]; then
#		logger_info "Disabling existing y2pmsh sources."
#		y2pmsh source -d $sources
#	fi

	if $install $virtualminmeta; then
		logger_info "Installation completed."
		logger_debug "$install returned: $?"
	else
		fatal "Installation failed: $?"
	fi
#	if [ "$sources" != "" ]; then
#		logger_info "Re-enabling any pre-existing sources."
#		y2pmsh source -e $sources
#	fi

	logger_info "If you are not regularly updating your system nightly using yum or up2date"
	logger_info "we strongly recommend you update now, using yast."
	return 0
}

install_with_rug () {
	logger_info "Installing Virtualmin and all related packages now using the command:"
	logger_info "$install $virtualminmeta"

	if $install $virtualminmeta; then
		logger_info "Installation completed."
		logger_debug "$install returned: $?"
	else
		fatal "Installation failed: $?"
	fi
	return 0
}

install_with_urpmi () {
	logger_info "Installing Virtualmin and all related packages now using the command:"
	logger_info "urpmi $virtualminmeta"

	if urpmi $virtualminmeta; then
		logger_info "Installation of $virtualminmeta completed."
	else
		fatal "Installation failed: $?"
	fi

	logger_info "If you are not regularly updating your system nightly using urpmi"
 	logger_info "we strongly recommend you update now, using the following commands:"
	logger_info "urpmi.update -a"
	logger_info "urpmi --auto-select"
	return 0
}

install_deps_the_hard_way () {
	logger_info "Installing dependencies using command: $install $deps"
	if runner "...in progress, please wait..." "$install $deps"
	then return 0
	else
		fatal "Something went wrong during installation: $?"
	fi
	exit $?
}

install_virtualmin () {
	case $package_type in
		rpm)
			case $os_type in
				suse)
					case $os_version in
						9.3|10.0)
							install_with_yast
						;;
						10.1|10.2)
							install_with_rug
						;;
					esac
				;;
				mandr*)
					install_with_urpmi
				;;
				*)
					install_with_yum
				;;
			esac
		;;
		deb)
			install_with_apt
		;;
		ebuild)
			install_with_emerge
		;;
		*)
			install_with_tar
		;;
	esac
	return 0
}

# virtualmin-release only exists for one platform...but it's as good a function
# name as any, I guess.  Should just be "setup_repositories" or something.
install_virtualmin_release
# We have to use $install to pre-install all deps, because some systems don't
# cooperate with our repositories (that's RHEL and SUSE 10.1, so far).
if [ "$mode" = "full" ]; then
	install_deps_the_hard_way
	success
fi

install_virtualmin

# We want to make sure we're running our version of packages if we have
# our own version.  There's no good way to do this, but we'll 
logger_info "Checking for updates to Virtualmin-related packages..."
if runner "...in progress, please wait..." "$install $deps"; then
	success
else
	logger_info "There may have been a problem updating some packages."
fi

# Functions that are used in the OS specific modifications section
disable_selinux () {
	seconfigfiles="/etc/selinux/config /etc/sysconfig/selinux"
	for i in $seconfigfiles; do
		if [ -e $i ]; then
			sed -i "s/SELINUX=.*/SELINUX=disabled/" $i
		fi
	done
}

# Changes that are specific to OS
case $os_type in
	"fedora" | "centos" | "rhel"  )
		disable_selinux
	;;
esac

exit 0

#!/bin/sh

# you can call this script to perform chron jobs
# # daily at 1 minute before midnight
# 59 23 * * * /home/nicos/bin/pack_dnloads.sh daily >> /tmp/pack_dnloads_daily_log.txt 2>&1
# # weekly 1 minute before Sunday (end of week = Saturday)
# 59 23 * * 6 /home/nicos/bin/pack_dnloads.sh weekly >> /tmp/pack_dnloads_weekly_log.txt 2>&1
# # monthly, at the very beginning (first min of first day) as i think is hard to pinpoint end of
# 0 1 1 * *   /home/nicos/bin/pack_dnloads.sh monthly >> /tmp/pack_dnloads_monthly_log.txt 2>&1

case $# in
	0) tog=""
	;;
	1)
      if (test $1 = -h)
         then
	      echo 'usage:' $0 ' -h'
	      echo 'usage:' $0 ' Psfx '
         echo '        Psx is used to create name of tracker file: user_app_data(stoics/pack_dnloads/pack_downloads-Psfx.csv'
         echo '        for example: $0 daily  -> pack_downloads-daily.csv'
         echo '        when no Psfx is give output file is pack_dnloads.csv'
	      exit
      fi
	   tog=-$1
   ;;
	*) echo 'usage:' $0 ' -h'
	   echo 'usage:' $0 ' Psfx'
	exit
esac

# Set your environment, as cron jobs are likely run on a different environment than your login shell.
# The following is a maximal set of variables, you can probably get away with a much smaller set
export LD_LIBRARY_PATH=/usr/local/users/nicos/local/lib/R/lib:/usr/local/users/nicos/local/lib64/:/usr/local/users/nicos/local/git/lib64/:/usr/local/users/nicos/local/git/lib/:/usr/local/users/nicos/local/lib/:/lib64/:/usr/lib64/:/lib/:/usr/lib/
export LDFLAGS="-Wl,-rpath,/usr/local/users/nicos/local/lib/R/lib"
export local=/usr/local/users/nicos/local
export PATH=/usr/local/users/nicos/local/git/bin:/usr/local/users/nicos/local/bin:/home/nicos/bin:/usr/local/bin:/usr/local/sbin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games:/snap/bin
export R_HOME=/usr/local/users/nicos/local/lib/R
export R_LIB=/usr/local/users/nicos/local/lib/R/library
export PKG_CONFIG_PATH=/usr/local/users/nicos/local/lib/R/library/pkgconfig

/usr/local/users/nicos/local/git/bin/upsh p /home/nicos/.local/share/swi-prolog/pack/b_real/scripts/pack_dnloads.pl mode=update tracker=user_app_data="stoics/pack_dnloads/pack_dnloads$tog.csv"

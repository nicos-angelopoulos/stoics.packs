#!/bin/sh

case $# in
	0) tog=""
	;;
	1)
      if (test $1 = -h)
         then
	      echo 'usage:' $0 ' -h'
	      echo 'usage:' $0 ' Psfx '
         echo '        Psx is used to create name of tracker file: user_app_data(stoics/pack_dnloads/pack_downloads-Psfx.txt'
         echo '        for example: $0 daily  -> pack_downloads-daily.txt'
         echo '        when no Psfx is give output file is pack_downloads.txt'
	      exit
      fi
	   tog=-$1
   ;;
	*) echo 'usage:' $0 ' -h'
	   echo 'usage:' $0 ' Psfx'
	exit
esac

# The following is a maximal set of variables, you can probably get away with a much smaller set
export LD_LIBRARY_PATH=/usr/local/users/nicos/local/lib/R/lib:/usr/local/users/nicos/local/lib64/:/usr/local/users/nicos/local/git/lib64/:/usr/local/users/nicos/local/git/lib/:/usr/local/users/nicos/local/lib/:/lib64/:/usr/lib64/:/lib/:/usr/lib/
export LDFLAGS="-Wl,-rpath,/usr/local/users/nicos/local/lib/R/lib"
export local=/usr/local/users/nicos/local
export PATH=/usr/local/users/nicos/local/git/bin:/usr/local/users/nicos/local/bin:/home/nicos/bin:/usr/local/bin:/usr/local/sbin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games:/snap/bin
export R_HOME=/usr/local/users/nicos/local/lib/R
export R_LIB=/usr/local/users/nicos/local/lib/R/library
export PKG_CONFIG_PATH=/usr/local/users/nicos/local/lib/R/library/pkgconfig

/usr/local/users/nicos/local/git/bin/upsh p /home/nicos/.local/share/swi-prolog/pack/b_real/scripts/pack_dnloads.pl mode=update tracker=user_app_data="stoics/pack_dnloads/pack_downloads$tog.txt"

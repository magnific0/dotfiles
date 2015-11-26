#!/usr/bin/bash
cd ~/Gits/ASynK
OCUSERPASS="$(gpg --decrypt ~/.owncloud.gpg)"
OCUSER="$(echo ${OCUSERPASS} | awk '{print $1}')"
OCPASS="$(echo ${OCUSERPASS} | awk '{print $2}')"
unset -v OCUSERPASS
/usr/bin/expect <<EOD
spawn python2 ./asynk.py --op=sync --name bbown
stty -echo
expect "Please enter username for"
send "${OCUSER}\n" 
expect "Enter password for"
send "${OCPASS}\n"
stty echo
expect { 
    timeout { exp_continue }
    eof
}
EOD
unset -v OCUSER
unset -v OCPASS

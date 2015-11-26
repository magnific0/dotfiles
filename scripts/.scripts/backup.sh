
#!/bin/sh
#

#
# Main program
#
backup_dir="/home/wacko/Backup"
cd "$backup_dir"
byellow="\e[1;33m"
bblack="\e[1;30m"
normal="\e[0m"
umask 0077

host="$(hostname -s)"
if [ ! -e "$host.include" ]; then
  echo "configfile \"$host.include\" not found."
  exit 1
fi

date_norm="$(date '+Week %W, %c')"
date_file="$(date '+%y%W_%m%d%H%M')"
series_number="$(date '+%y%W')"

incfile="$host.$date_file.incremental"
if [ ! -f "$host.$series_number.snar" ]; then
  level="full 0-backup"
  tarball="$host.$date_file-full.tar.gz.gpg"
  incfile="$host.$series_number.snar"
  # delete all older archives
  echo "Deleting $host.$(expr $series_number - 2)*"
  rm -rf $host.$(expr $series_number - 2)*
else
  level="incremental level 1 backup"
  tarball="$host.$date_file.tgz"
  incfile="$host.$date_file.snar"
  cp "$host.$series_number.snar" "$incfile"
fi
echo "$level"

# exclude-datei: slash am anfang ist ok, ABER NICHT AM ENDE!
# -> /path/to/dir
# -> /home/*/.mozilla
exclude=""
[ -f "$host.exclude" ] && exclude="-X $host.exclude"

logfile="$host.$date_file.log"

if [ -f "$tarball" ]; then
  echo "$tarball already exists"
  exit 1
fi

# info
echo -e "${bblack}tarball \t: ${byellow}$tarball"
echo -e "${bblack}incfile \t: ${byellow}$incfile"
echo -e "${bblack}logfile \t: ${byellow}${logfile} ${bblack}"
echo
echo -e "${bblack}-- directories --${byellow}"
cat "$host.include"
echo
if [ -n "$exclude" ]; then
  echo -e "${bblack}-- excludes --${byellow}"
  cat "$host.exclude"
fi
echo -e "$normal"
# end info

[ -f "$host.pre.sh" ] && ./$host.pre.sh

chmod 600 "$incfile"
time nice -n 19 tar --exclude-caches --label="$date_norm: $level" --totals -czg "$incfile" -f - $exclude $(< "$host.include") | gpg --batch --yes --passphrase-fd 3 -c 3< ~/.backuppass > "$tarball"
[ -f "$host.post.sh" ] && ./$host.post.sh

chmod 400 "$incfile" "$tarball" 

# Send files to remote server
echo "Syncing files with remote backup server..."
rsync -a -v -e "ssh -p 5022" --ignore-existing --delete "$backup_dir"/"$host"* "$host"@geul.net:/volume1/NetBackup/"$host"

if [ $? -eq 0 ]; then
    echo "backup successful ($?)"
    exit 0
else
    echo "errors or warnings occured ($exitcode)"
    echo
    sed '/^tar/!d; /is new/d' "$logfile"
    exit 1
fi

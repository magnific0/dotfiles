#!/bin/bash
# FETch LArgest MAtch
# Download the largest reverse image match of a linked image
# Usage:
# fetlama.sh http://url.to/image.jpg

log () {
    echo "${@}" >> /tmp/fetlama.log
}

# First save the original, maybe there are no matches or all bad ones, this way we ensure at least a single copy
OLD_IMG=`wget -nv "$@" 2>&1| sed 's/[0-9]\{4\}-[0-9]\{2\}-[0-9]\{2\}[[:blank:]]\+[0-9:]\{8\}[[:blank:]]\+URL[^ ]\+ \[[0-9\/]\+\][[:blank:]]\+->[[:blank:]]\+"\([^"]\+\)".*$/\1/'`
log "ORIGINAL: " $@

# Fetch result page with largest size (large,med,small), first supply Google with image URL, get image fingerprint back, use that to retrieve results
# TODO: store fingerprint, match against already retrieved images, saves requests. If 100% use second best match, perhaps user can than reinitiate the command if the first contains a watermark or so.
LIMGURL=http://www.google.com`wget --user-agent="Mozilla/5.0 (X11; U; Linux i686; en-US; rv:1.9.0.3) Gecko/2008092416 Firefox/3.0.3" -qO- "http://images.google.com/searchbyimage?image_url=$@" | ~/.scripts/list_urls.sed | grep -e isz: | head -1 | sed 's/amp;//g'`"&biw=1&bih=1"
FILENAME=`echo $OLD_IMG | awk -F'.' '{print $1}'`
FINGERPRINT=`echo "$LIMGURL" | awk -F',isz' '{print $1}' | awk -F'simg:' '{print $2}'`
log "FINGERPRINT: " $FINGERPRINT

# Get the results page
wget --user-agent="Mozilla/5.0 (X11; U; Linux i686; en-US; rv:1.9.0.3) Gecko/2008092416 Firefox/3.0.3" -O /tmp/$FILENAME.output.html "$LIMGURL" -q

# Extract all the Image URLs of the Matches, along with width and source URL
cat /tmp/$FILENAME.output.html | grep imgres?imgurl | ~/.scripts/list_urls.sed | grep imgres | awk -F'&amp;sz' '{print $1}' | sed 's/&amp;imgrefurl=/|/g' | sed 's/\/imgres?imgurl=//g' | sed 's/&amp;usg=\(.*\)&amp;w=/|/g' | awk -F'|' '{print $3 "|" $1 "|" $2}' | sort -r > /tmp/$FILENAME.sources

# Get the top match, SRC is unused, but can be used if message is reposted (for proper references)
i=1
while true; do
    IMG_URL=`sed -n "$i p" /tmp/$FILENAME.sources | awk -F'|' '{print $2}'`
    if [ -z "$IMG_URL" ]; then break; fi
    IMG_SRC=`sed -n "$i p" /tmp/$FILENAME.sources | awk -F'|' '{print $3}'`
    log "IMG:" $IMG_URL " FROM:" $IMG_SRC
    NEW_IMG=`wget -nv "$IMG_URL" 2>&1| sed 's/[0-9]\{4\}-[0-9]\{2\}-[0-9]\{2\}[[:blank:]]\+[0-9:]\{8\}[[:blank:]]\+URL[^ ]\+ \[[0-9\/]\+\][[:blank:]]\+->[[:blank:]]\+"\([^"]\+\)".*$/\1/'`
    i=`expr $i + 1`
    if file -ib "$NEW_IMG" | grep image > /dev/null; then
	break
    fi
done
mv $NEW_IMG $FILENAME.$NEW_IMG
exit

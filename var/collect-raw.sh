#!/bin/bash

ffmpeg -f avfoundation -list_devices true -i "" 2>&1 |tail -7

dev_idx=`ffmpeg -f avfoundation -list_devices true -i "" 2>&1 |tail -7|grep 'Studio 24c'|awk -F'[' '{print $3}'|awk -F']' '{print $1}'`

echo "using device idx $dev_idx"

chans=2
srate=192000
seconds=2
outfile="out.raw"

#/usr/local/bin/ffmpeg -f avfoundation -ac 2 -i :1 -t 20 -y -hide_banner -nostats -nostdin longer-ffmpeg.wav
#/usr/local/bin/ffmpeg -f avfoundation -ac 2 -i :1 -ar 192000  -t 20 -y -hide_banner -nostats -nostdin longer-ffmpeg-192.wav

# -f s16le -c:a pcm_s16le

# need pcm codec to specify channel order into file
# http://trac.ffmpeg.org/wiki/audio%20types

# -f avfoundation

/usr/local/bin/ffmpeg -f avfoundation -i :$dev_idx -f s24le -ac $chans -ar $srate -t $seconds -y \
	-hide_banner -nostats -nostdin -f s24le -c:a pcm_s24le ${outfile}


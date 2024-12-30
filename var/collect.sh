/usr/local/bin/ffmpeg -f avfoundation -ac 2 -i :1 -t 20 -y -hide_banner -nostats -nostdin longer-ffmpeg.wav
/usr/local/bin/ffmpeg -f avfoundation -ac 2   -i :1 -ar 192000  -t 20 -y -hide_banner -nostats -nostdin longer-ffmpeg-192.wav

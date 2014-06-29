#echo "samim" >> /home/samim/whh
/bin/aria2c --conf-path="$HOME/.aria2/aria2.conf" -i "$HOME/Downloads/nightly/downloads.txt" -d "$HOME/Downloads/nightly/" -l "$HOME/Downloads/nightly/log"


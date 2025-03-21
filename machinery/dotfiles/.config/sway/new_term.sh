current_pid=$(swaymsg -r -t get_tree | grep 'focused": true' -A 50 \
  | grep pid | grep [0-9]* -o )

# not necessarily the pid of bash...
# just the first child-process of current_pid
bash_pid=$(ps -o pid= --ppid $current_pid | grep [0-9]* -o -m 1 )

if [[ $bash_pid ]]; then
  foot -D $(realpath /proc/$bash_pid/cwd )
else
  foot
fi

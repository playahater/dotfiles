if [[ -z $DISPLAY && ! -e /tmp/.X11-unix/X0 ]] && (( EUID )); then
  exec xinit -- /usr/bin/X -nolisten tcp vt7
fi

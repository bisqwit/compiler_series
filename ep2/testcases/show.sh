echo -ne '\033[2J\033[H'
echo -e '\033[0;38;5;100mbisqwit\033[0;38;5;101m@\033[0;38;5;106mchii\033[0;38;5;70m:~/com_ep2/testcases\033[0;38;5;71m$\033[m ./show.sh '"$*"
s='=\033[1C\033[1D'
s="$s""$s"
s="$s""$s"
s="$s""$s"
s="$s""$s"
s="$s""$s"
s="$s""$s"
s="$s""$s"
s="$s""$s"
s='\033[0;38;5;12;48;5;235m\033[K\r'"$s"

echo -ne "$s"
echo -ne '\r\033[2C\033[37m[ Original Code ]'
echo -e '\033[m'
  #echo
  (cd .. ; ../vid-ep1/ccat testcases/$1|grep -v '//' )
  echo
echo -ne "$s"
wid="$(../conj "$1" | tr -d '\012' |sed  's/.*//'|wc -c)"
wid=$((wid+2))
echo -ne '\r\033[2C\033[37m[ Parse Tree ]'
echo -ne '\r\033['$wid'C\033[37m[ Optimized ]'
echo -e '\033[m'
../conj $1

x=108
y=3
for s in 0*.code2; do
  echo -ne "\033[$y;$x"H"\033[K"
  echo -ne '\033[0;38;5;12;48;5;235m|\033[m '
  if [ "$s" = "$1" ]; then
    echo -ne '\033[30;47m'
  fi
  echo -n "$s"
  echo -e '\033[m'
  y=$((y+1))
done
while [ $y -ne 50 ]; do
  echo -ne "\033[$y;$x"H"\033[K"
  echo -e '\033[0;38;5;12;48;5;235m|\033[m '
  y=$((y+1))
done

echo -ne '\033[0;38;5;100mbisqwit\033[0;38;5;101m@\033[0;38;5;106mchii\033[0;38;5;70m:~/com_ep2/testcases\033[0;38;5;71m$\033[m '

echo -ne '\033[8;50;133t'
f=""

p=""
files="$(ls *.code2)"

#p="_"
#files="$(ls {007,008a,008b,010a,010b,011}*.code2)"
#files="$(ls 011*.code2)"

for s in $files;do ./show.sh $s;import $s"$p".png;f="$f $s"$p".png";done
#echo "Resizing... Press any key"
#read s
echo 'Resizing!'
parallel -j4  mogrify -filter point -quality 100 -resize 800%  -- $f
echo "Done"

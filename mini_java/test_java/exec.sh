rm *.s
for X in *.java; do
    echo $X
    ./main.byte $X
    read
done
for Y in *.s; do
    echo $Y
    ./Mars_4_2.jar $Y
    read
done

echo "Starting generation ..."

[[ $# -ne 1 ]] && echo "Invalid input, expecting number" && exit 1

re='^[0-2][0-9]$'
! [[ $1 =~ $re ]] && echo "Invalid number input, expecting in range 01 - 25" && exit 1

mkdir day-$1
cp template.hs day-$1/Main.hs
touch day-$1/input.txt day-$1/example.txt

echo "Done"

IFS=
export $(grep -v '#.*' .env | xargs)

if test -f src/main/scala/Day$1.scala; then
  echo "File exists. Exiting..."
  exit 1
fi

curl -b "session=$GITHUB_TOKEN" https://adventofcode.com/2024/day/$1/input > src/inputs/actual/day$1.txt
touch src/inputs/test/day$1.txt

cp src/main/scala/DayX.scala src/main/scala/Day$1.scala
cp src/test/scala/DayX.scala src/test/scala/Day$1.scala

# fix up module names and imports
sed -i "" "s,DayX,Day$1,g" "src/main/scala/Day$1.scala"
sed -i "" "s,DayX,Day$1,g" "src/test/scala/Day$1.scala"
sed -i "" "s,// new days added here,import Day$1.{ solve => day$1 }\n// new days added here,g" "src/main/scala/Main.scala"
sed -i "" "s,// mappings added here,$1 -> day$1\,\n    // mappings added here,g" "src/main/scala/Main.scala"
sed -i "" "s,aoc2024(1,aoc2024($1,g" "src/test/scala/Day$1.scala"
sed -i "" "s,.ignore,,g" "src/test/scala/Day$1.scala"
        
sbt
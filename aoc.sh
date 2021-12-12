#!/bin/bash

NUMBER=$1
DAY=$1

if [ ${#DAY} == 0 ]; then
  echo "Give me a day number!"
  exit 1
fi

if [ ${#DAY} == 1 ]; then
  DAY="0$DAY"
fi

FOLDER="day$DAY"

if [ ! -d "day$DAY.1" ] 
  then
    FOLDER="$FOLDER.1"
    #cp -an template/structure $FOLDER
    mkdir $FOLDER
    pushd $FOLDER && cabal init -n -a "Paul Sanford" -p "Day$NUMBER" --libandexe && rm CHANGELOG.md && rm src/MyLib.hs && popd
    day_number=$NUMBER mustache ENV template/Day.mustache > "$FOLDER/app/Main.hs"
    day_number=$NUMBER mustache ENV template/Lib.mustache > "$FOLDER/src/Aoc.hs"
    sed -i 's/MyLib/Aoc/g' "$FOLDER/Day$NUMBER.cabal"
    day_number=$NUMBER mustache ENV template/Makefile.mustache > "$FOLDER/Makefile"
    pushd $FOLDER && cabal-edit add $(<../template/deps.txt) && popd
    pushd $FOLDER && cabal build && cabal clean && popd
    echo "Scaffolded folder $FOLDER"
  elif [ ! -d "day$DAY.2" ]
  then
    FROM="$FOLDER.1"
    TO="$FOLDER.2"
    cp -an $FROM $TO
    pushd $TO && cabal build && cabal clean && popd
    echo "Copied $FROM to $TO"
  else
    echo "Both folders exist!"
    exit 2
fi


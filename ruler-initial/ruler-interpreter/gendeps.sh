#!/bin/bash

echo "# Generated AG dependencies" > Makefile.depend

for file in $@
do
  output=`cd src; $UUAGC --genfiledeps $file.ag | tr '\n' ' '`
  echo "derived/$file.hs: src/$file.ag \$(addprefix src/, $output)" >> Makefile.depend
  echo "Makefile.depend: src/$file.ag \$(addprefix src/, $output)" >> Makefile.depend
done


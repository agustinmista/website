#!/bin/bash

set -e

# ----------------------------------------
# Project settings

# The compilation log file
LOGFILE=thesis.log

# Existing .bib files
BIBFILES=$(ls bib/*.bib)

# Output .bib file
OUTPUT=references.bib

# ----------------------------------------
# Parse the missing references

MISSING=$( \
  grep "LaTeX Warning: Citation" $LOGFILE |\
  awk -F' ' '{print $4}' | tr -d \' |\
  sort | uniq \
)

# ----------------------------------------
# BibTool goes brrr

find_ref () {
  bibtool -q -r biblatex '--expand.macros=ON' '--print.all.strings=OFF' '--select{$key "'$1'"}' $2
}

for missing in $MISSING; do
  echo -n "Looking for $missing: "

  # Reference is already there
  FOUND=$(find_ref $missing $OUTPUT)
  if [ ! -z "$FOUND" ]; then
    echo "Already in in $OUTPUT!"
    continue
  fi

  # Reference is in some .bib file
  for bibfile in $BIBFILES; do
    FOUND=$(find_ref $missing $bibfile)
    if [ ! -z "$FOUND" ]; then
      echo "Found in $bibfile!"
      echo "$FOUND" >> $OUTPUT
      break
    fi
  done

  # Reference is M.I.A.
  if [ -z "$FOUND" ]; then
    echo "Could not find any entry!"
  fi

done
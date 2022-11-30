---
title: Dusting the bookshelf
date: 2022-11-30
---

I'm currently in the process of writing my Ph.D. thesis. This means I need to gather all my published papers and fight against LaTeX and Lhs2TeX until everything fits together and looks somewhat decent. One of the many challenges of this process was to deal with the multiple bibliography (`.bib`) files I collected over the years. Hopefully I'm not the only one in this situation:

* Paper $0$: inherit a gigantic `.bib` from advisor and add new references as needed
* Paper $n$: pick the `.bib` from some paper $k$ (with $0 \leq k \lt n$) and add new references as needed

So here I was, sitting with ~50kLOC worth of `.bib` files I needed to organize. On the other hand, I wanted to collect all the references in a single section of the thesis to avoid repetetition and inconsistencies. So, in principle I could have simply used the good 'ol `bibtool` utility to merge all my `.bib` files together:

```bash
$ bibtool -s foo.bib bar.bib baz.bib
```

The problem with this approach is that I would have ended up again with a gigantic `.bib` file when, in reality, I only needed a handful of entries from it. To solve this, I put together a small bash script that pulls references from `.bib` files on demand. The trick is to ask for forgiveness rather than for permission:

1. Compile the project with an empty `.bib` file
2. Parse the log file to find which citations are missing references
3. Extract each of those references from some existing `.bib` file and append them to the project's `.bib` file
4. Compile the project again, now without missing references :D

## Finding missing citations

If we inspect the log files created by `pdflatex`/`bibtex`, missing citations are reported as:

```bash
LaTeX Warning: Citation 'afl' on page 21 undefined on input line 16664.
```

So we need to find these warnings, extract the citation keys (i.e., `afl` in the line above) from them and remove any duplicates:

```bash
LOGFILE=thesis.log

MISSING=$( \
  grep "LaTeX Warning: Citation" $LOGFILE |\
  awk -F' ' '{print $4}' | tr -d \' |\
  sort | uniq \
)
```

With the culprits at hand, we can move onto the next step.

## Retrieving citations on demand

To find if a concrete reference exists in a given citation file, we can use `bibtool`'s `select` resource:

```bash
find_ref () {
  bibtool -q -r biblatex '--expand.macros=ON' '--print.all.strings=OFF' '--select{$key "'$1'"}' $2
}
```

This way, `find_ref $key $bibfile` will try to find the reference `$key` in the file `$bibfile`, returning either its corresponding entry if it can find it, or an empty string otherwise. The `-r biblatex` option is needed to allow indexing `@online` entries, whereas the `expand.macros` and `print.all.strings` options are needed to ensure that string macros are expanded, so we don't need to print them everytime we look for a citation key.

Then, we can iterate over the existing `.bib` files to see if we find each missing citation:

```bash
BIBFILES=$(ls bib/*.bib) # Existing .bib files
OUTPUT=references.bib # The project's .bib file

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
```

The reason we first look in the project's `.bib` file is to avoid producing duplicate entries when:

* The user already added them manually, or
* We have an overlapping entry key that matches a previously searched missing reference. [^1]

## Trying it out

We can now run this little snippet to gather only the references we need:

```
Looking for afl: Found in bib/foo.bib!
Looking for asan: Found in bib/bar.bib!
Looking for peach: Found in bib/baz.bib!
...
```

And my curated `.bib` file now has "only" a little over 2000 lines of code ¯\\\_(ツ)\_/¯

You can find the bash script [here](../assets/code/find_references.sh) in case you want to give it a try.

[^1]: This happens because `bibtool`'s `select` resource looks for **all** the entries whose keys **contain** the given key string. So, for instance, both keys `foo` and `foobar` will match with `foo`, possibly returning more than one entry per `find_ref`. If you know how to avoid this, please let me know!
---
title: Syntax test
date: 2018-06-03
---

This post is mostly intended to test the Markdown syntax supported by Pandoc
with the page Css.

## Plain Text

Aliquam erat volutpat. Nunc eleifend leo vitae magna. In id erat non orci
commodo lobortis. Proin neque massa, cursus ut, gravida ut, lobortis eget,
lacus. Sed diam. Praesent fermentum tempor tellus. Nullam tempus. Mauris ac
felis vel velit tristique imperdiet. Donec at pede. Etiam vel neque nec dui
dignissim bibendum. Vivamus id enim. Phasellus neque orci, porta a, aliquet
quis, semper a, massa. Phasellus purus. Pellentesque tristique imperdiet tortor.
Nam euismod tellus id erat.

--------------------------------------------------------------------------------

## Links

* [Here](https://agustinmista.github.io) is a link.
* [Here](https://agustinmista.github.io "Some text!") is the same link with alt
  text.

--------------------------------------------------------------------------------

## Images

Images are inserted using the following syntax. Captions are optional.

![This is a caption.](/assets/img/lambda.png){ width=20% }

## Math

$N_{f}$, the flux associated with the $i$th filter is $F_{i}$, and the weight,
$w_{i}$, is defined as

$$w_{i}\equiv\sigma_{F,i}^{-2},$$

where $\sigma_{F,i}$ is the uncertainty in the flux associated with the $i$th filter.
Assume that all sums range from $i=1$ to $i=N_{f}$ and use the summation convention
on repeated indices. Note that we strike things out like ~~this~~, italicize things
*like this* or _like this_, and we can make things bold **like this** or __like this__.

Unfortunately, --> and => do not turn into arrows automatically, but you can make
arrows like this $\rightarrow$ and $\Rightarrow$. You can also implement a Pandoc
[filter](http://pandoc.org/scripting.html#json-filters) to add features that are not
built-in. So, it would be possible to make --> turn into $\rightarrow$ and introduce
new features, such as [Graphviz](http://www.graphviz.org/) graph rendering.
For examples of such filters, check out
[this list](https://github.com/jgm/pandoc/wiki/Pandoc-Filters) (or keep reading this
document).

Here's a horizontal line / separator:

------------

## Table Creation

There are many ways of creating tables. This is an example of the pipe table
creation syntax...

| Label        | Description                                                         |
| -----------: | :------------------------------------------------------------------ |
| `meanflx`    | $\langle F \rangle = F_i$                                           |
| `wmeanflx`   | $\langle F\rangle_w = \omega$                                       |

Here's one more example, with a table caption:


  Right  Left     Center
 ------  ------  --------
     12  12        12
    123  123       123
      1  1          1
 --------------------------

Table:  Demonstration of simple table syntax.


Refer to the [documentation](http://pandoc.org/MANUAL.html#tables) to learn more
about the other types of tables.

--------------

## Line Blocks

Normally, adjacent lines are combined together, so a set of short formatted lines,
such as an address or verse, becomes jumbled. To overcome this, place `|` at the
beginning of each line, followed by a space. This preserves the formatting:

| I am the very model of a modern Major-General,
| I've information vegetable, animal, and mineral,
| I know the kings of England, and I quote the fights historical
| From Marathon to Waterloo, in order categorical
| I'm very well acquainted, too, with matters mathematical,
| I understand equations, both the simple and quadratical,
| About binomial theorem I'm teeming with a lot o' news,
| With many cheerful facts about the square of the hypotenuse.
|
| I'm very good at integral and differential calculus;
| I know the scientific names of beings animalculous:
|     In short, in matters vegetable, animal, and mineral,
|     I am the very model of a modern Major-General.
|

Note the indentation on the final two lines above.


----------------------------------------------

## Block Quotes

The syntax for block quotes is extremely simple:

> You can write a block quote by putting a single '`>`' at the beginning of a block of text
or by placing the '`>`' at the beginning of each line of the quoted block, similar
to the way e-mail readers handle quoted messages.

------------------------------------------------

## Enumerated lists

#. You can use integers or the `#` symbol in enumerated lists.
#. This is quite convenient. For example,
       i. You don't have to count
      ii. If you want to change the order, no numbering needs to be changed
#. You can also use roman numerals, obviously.

## Example Lists

(@first)  This is Example (@first).

(@second) This is Example (@second).

Now we discuss something for a while and introduce the third example...

(@bla) This is Example (@bla).

You can refer to an example by its label. For instance Example (@second).

-------------------------------------------------

## Syntax Highlighting

### A Python syntax highlighting example:

You can include blocks of pre-formatted text, like this. If the text is source code,
you can tell Pandoc to perform syntax highlighting. This is graphviz.py:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.haskell}
{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid ((<>))

import System.FilePath
import Hakyll


main :: IO ()
main = hakyll $ do

  match "images/*" $ do
    route   idRoute
    compile copyFileCompiler

  match "css/*" $ do
    route   idRoute
    compile compressCssCompiler

  match "templates/*" $ compile templateBodyCompiler

  match "index.md" $ do
    route   $ setExtension "html"
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/default.html" defaultContext
      >>= relativizeUrls
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### A C++ syntax highlighting example:

Here's another example using a different programming language (and a different
method of specifying the pre-formatted text block).

``` {.cpp}
#include <iostream>

int main()
{
    // comment
    std::cout << "Hello, World!\n";
    return 0;
}

```

----------------------


## Compiling this with Pandoc

To convert[^1] this Markdown document to HTML, I used the following command:

``` {.bash}
# BTW, this is a Bash syntax highlighting example.

$ pandoc example.md -s --smart --mathjax \
         --css nrstyle.css \
         --highlight-style pygments \
         --columns=200 \
         --filter graphviz.py \
         -o example.html
```


### An explanation of each parameter (as a bulleted list)


* **`example.md`**:  the input file
* **`-s`**: create a stand-alone document (rather than a document
    fragment that lacks a header).
* **`--smart`**: automatically replace `--`, `---`, and `...` with --, ---, and ...
    and handle quotation marks properly.
* **`--mathjax`**: use the [MathJax](https://www.mathjax.org/) library for
    typesetting math in HTML documents.
* **`--css nrstyle.css`**: Use the nrstyle.css stylesheet in the HTML output document.
* **`--highlight-style pygments`**: Turn on syntax highlighting and
    use the pygments color scheme.
* **`--columns=200`**: set the line length to 200. This makes the
    tables display properly.
* **`--filter graphviz.py`**: Pass Pandoc's abstract syntax tree through the filter
    program `graphviz.py` before rendering the output document.
* **`-o example.html`**: Specify the name and type of the output file.
    The format is inferred from the suffix (file extension).

----------------



### The same explanation, formatted as a definition list

`example.md`

:    The input file

`-s`

:    Create a stand-alone document (rather than a document fragment that lacks a header).

`--smart`

:   Automatically replace `--`, `---`, and `...` with --, ---, and ... and handle quotation marks properly.

`--mathjax`

:    Use the [MathJax](https://www.mathjax.org/) library for typesetting math in HTML documents.

`--css nrstyle.css`

:    Use the nrstyle.css stylesheet in the HTML output document.

`--highlight-style pygments`

:    Turn on syntax highlighting and use the pygments color scheme.

`--columns=200`

:    set the line length to 200. This makes the tables display properly.

`--filter graphviz.py`

: Pass Pandoc's abstract syntax tree through the filter program `graphviz.py` before rendering the output
document.

`-o example.html`

:    Specify the name and type of the output file. The format is inferred from the suffix (file extension).

[^1]: This is a footnote.


[//]: # This is a comment

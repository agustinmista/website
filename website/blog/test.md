---
title: Syntax test
date: 2018-06-03
---

A quick tour of the Markdown features this site supports.

## Plain text

Plain paragraph with *italic*, **bold**, and ~~strikethrough~~ text.

---

## Links

* [A link](https://agustinmista.github.io)
* [A link with a title](https://agustinmista.github.io "title")

---

## Images

![Caption](/assets/img/lambda.png){ width=20% }

---

## Math

Inline: $w_i \equiv \sigma_{F,i}^{-2}$.

Display:

$$ \langle F \rangle_w = \sum_{i=1}^{N_f} w_i F_i $$

---

## Tables

Pipe table:

| Label      | Description            |
| ---------: | :--------------------- |
| `meanflx`  | $\langle F \rangle$    |
| `wmeanflx` | $\langle F \rangle_w$  |

Grid table with caption:

  Right  Left   Center
 ------  -----  -------
     12  12        12
    123  123      123

Table:  Simple grid table.

---

## Line blocks

| I am the very model of a modern Major-General,
| I know the kings of England, and quote the fights historical
| From Marathon to Waterloo — in order categorical.

---

## Block quotes

> A block quote is just a paragraph prefixed with `>`.

---

## Lists

Enumerated with `#`:

#. First
#. Second
   i. Nested with roman numerals
  ii. Continued
#. Third

Example list with labels:

(@foo)  The first example.
(@bar)  The second example.

See Example (@bar) for details.

---

## Syntax highlighting

Haskell:

```haskell
main :: IO ()
main = putStrLn "hi"
```

C++:

```cpp
#include <iostream>
int main() { std::cout << "hi"; return 0; }
```

Bash:

```bash
echo "hi"
```

---

## Definition lists

`--smart`

:   Automatically replace `--`, `---`, and `...` with typographic punctuation.

`--mathjax`

:   Use MathJax to typeset math in the output.

---

## Footnotes

Pandoc supports footnotes like this[^1].

---

## Comments

[//]: # (Markdown comments like this one are ignored.)

[^1]: Footnote text appears at the bottom of the document.
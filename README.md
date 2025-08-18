# Synopsis

`fountain-parser` is a small parser library for the
<u>[<span style="font-family: serif">Fountain</span>](https://fountain.io/)</u>
screenplay format, fully supporting 1.1 version
<u>[syntax](https://fountain.io/syntax/)</u> and producing a simple,
easy to grok <span style="font-family: serif">AST</span>.

`fountain-parser` is written in
<u>[<span style="font-family: serif">Haskell</span>](https://haskell.org)</u> and it
uses the
<u>[<span style="font-family: serif">Megaparsec</span>](https://hackage.haskell.org/package/megaparsec)</u>
library for parsing.

# Disclaimer

Currently, this is *pre-alpha* software, not yet usable in productive
form.

This software is distributed under the *<span style="font-family: serif">BSD
Three-Clause license</span>*. See the <u>[LICENSE](run:./LICENSE)</u>
file for more details.

# Motivation

The “*Developers*” section of the Fountain site provides a link to a
<u>[parsing library](https://github.com/nyousefi/Fountain)</u> in
<span style="font-family: serif">Objective C</span>. This already presents a
portability issue: there *are* projects that make it possible to bridge
Objective C and Haskell, but they’re platform- or framework-specific. It
also employs a multi-pass stategy where every stage creates a modified
version of the source, and it’s heavily reliant on *Regular
Expressions*.

Thus, to create a light-weight, performant and portable solution, it’s
necessary to start from scratch.

`fountain-parser` aims to power a series of command-line utilities for
conversion from Fountain to a series of convenient formats, (`.OTF`,
`.TEX`) without intervention from thirds.

## My software already supports Fountain

Of course. The <u>[“*Apps*” section](https://fountain.io/apps/)</u> of
the Fountain site lists a few that also import or export the format.
**The caveat**: most are either cloud-based and/or proprietary. By
favoring (mostly) open formats, *fountain-parse* allows integration into
many <span style="font-family: serif">FLOSS</span> tools, helping the creation of
compound documents (such as production bibles) and entirely
non-proprietary workflows.

# Implementation Specifics

- As per spec:

  - This library expects Fountain text to be encoded in
    <span style="font-family: serif">UTF-8</span>.

  - Tabs are converted into **four** spaces.

  - Your line-spacing is respected.

  - Initial spaces are ignored everywhere except in non-`>centered<`
    action lines.

  - A line with two spaces doesn’t count as an empty line.

- All parsing functions expect `Text` inputs. File I/O is left to the
  application or framework.

- Formatting (boldface, underline) found in such entities as character
  names or scene headings is ignored.

- Vertical tabs and form-feed characters are interpreted as line
  changes. For vertical spacing, use multiple blank lines and/or the
  Fountain form feed character sequence (“`===`”) instead.

- Unicode spaces are turned into the vanilla space, except for the *hair
  space*, which is discarded.

- The parser discards notes, boneyards, sections and synopses. Only
  printable content is retained.

# Building

<span style="font-family: serif">GHC</span> 9.6.7 and
<span style="font-family: serif">Cabal</span> 3.0 (or greater) are required to
compile and run the test suite (once implemented.)

The project uses the `GHC2021` language default. While it might be
possible to compile it in earlier versions than 9.6.7, this default is
only available since 9.2.1., so that constitutes a hard version limit
for those who might wish to experiment.

Some of the included scripts require Linux or a Linux-like environment
(e.g.,
<u>[<span style="font-family: serif">MSYS2</span>](https://www.msys2.org/)</u>.)

# Contact

Please <u>[create an
issue](https://github.com/CubOfJudahsLion/fountain-parser/issues)</u> if
you find a bug.

I can be reached directly at
*<span style="font-family: serif">10951848+CübO̱fJúdãhsLîòn</span>* ă(t)
*<span style="font-family: serif">users/noreply/gīthụb/cȯm</span>* (without accents
and replacing slashes by periods.)

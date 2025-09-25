---
title: |
  fountain-parser README  
    
  version 0.1.0.0
---

# Synopsis

`fountain-parser` is a small parser library for the
<u>[<span style="font-variant: small-caps">Fountain</span>](https://fountain.io/)</u>
screenplay format, fully supporting 1.1 version
<u>[syntax](https://fountain.io/syntax/)</u> and producing a simple,
easy to grok <span style="font-variant: small-caps">AST</span>.

`fountain-parser` is written in
<u>[<span style="font-variant: small-caps">Haskell</span>](https://haskell.org)</u> and
it uses the
<u>[<span style="font-variant: small-caps">Megaparsec</span>](https://hackage.haskell.org/package/megaparsec)</u>
library for parsing.

# Disclaimer

Currently, this is *pre-alpha* software, not yet usable in productive
form.

This software is distributed *as-is* under the terms of the
<span style="font-variant: small-caps">BSD Three-Clause license</span>. See the
<u>[`LICENSE`](run:./LICENSE)</u> file for more details.

# Motivation

The <u>[*Developers* section](https://fountain.io/developers/)</u> of
the Fountain site provides a link to a <u>[parsing
library](https://github.com/nyousefi/Fountain)</u> in
<span style="font-variant: small-caps">Objective C</span>. This presents a portability
issue: while there *are* projects that make it possible to bridge
<span style="font-variant: small-caps">Objective C</span> and
<span style="font-variant: small-caps">Haskell</span>, they’re platform- or
framework-specific. That library informs this project in matching the
different Fountain entities even as it uses different parsing methods.

## Prospective Related Projects

`fountain-parser` aims to power a series of command-line utilities for
conversion from <span style="font-variant: small-caps">Fountain</span> to a series of
convenient formats (like `.tex`) without intervention from thirds.

## My software already supports <span style="font-variant: small-caps">Fountain</span>

The <u>[*Apps* section](https://fountain.io/apps/)</u> of the
<span style="font-variant: small-caps">Fountain</span> site lists software that also
imports or exports the format. There’s a caveat: most are *cloud-based*
and/or *proprietary*. By favoring (mostly) open formats,
*fountain-parse* allows integration into many
<span style="font-variant: small-caps">FLOSS</span> tools, enabling entirely
non-proprietary workflows and helping the creation of compound documents
such as production bibles.

# Implementation Specifics

In general, the library parser is rather lenient, allowing liberal
spacing and recognizing <span style="font-variant: small-caps">Unicode</span>
codepoints. Languages without uppercase/lowercase distinction must
resort to *power-user characters* for case-dependent items such as
transitions (‘`>`’) and character names (‘`@`’).

- As per the <u>[syntax guide](https://fountain.io/syntax/)</u>:

  - This library expects <span style="font-variant: small-caps">Fountain</span> text to
    be encoded in <span style="font-variant: small-caps">UTF-8</span>.

  - Tabs are converted into **four** spaces.

  - Your line spacing is respected.

  - Initial spaces are ignored everywhere except in action lines.

  - A line with two spaces doesn’t count as an empty line.

- All parsing functions expect `Text` inputs. File I/O is left to the
  application or framework.

- Varying-width <span style="font-variant: small-caps">Unicode</span> spaces are either
  converted into regular spaces or suppressed if they’re hairline- or
  zero-width.

- Vertical tabs and form-feed characters are interpreted as line
  changes. For vertical spacing, use multiple blank lines and/or the
  <span style="font-variant: small-caps">Fountain</span> form feed character sequence
  (“`===`”) instead.

- The parser keeps everything: notes, boneyards, sections and synopses.
  Some possible conversion targets have analogues to those, so it might
  be desirable to preserve them.

## Tentative Grammar

The following is an attempt to formalize the syntax in
<u>[<span style="font-variant: small-caps">ABNF</span>](https://datatracker.ietf.org/doc/html/rfc5234)</u>,
drawing from the <u>[syntax guide](https://fountain.io/syntax/)</u> and
<span style="font-variant: small-caps">Objective C</span>
<u>[implementation](https://github.com/nyousefi/Fountain)</u>.

# Building

<span style="font-variant: small-caps">GHC</span> 9.6.7 and
<span style="font-variant: small-caps">Cabal</span> 3.0 (or greater) are required to
compile the library and run the tests (*not implemented yet.*)

The project uses the `GHC2021` language default. While it might be
possible to compile it in earlier versions than 9.6.7, this default is
only available since 9.2.1, constituting a hard limit.

Some of the included scripts require `make`, `sed` and other similar
utilities usually found in <span style="font-variant: small-caps">Linux</span> or
<span style="font-variant: small-caps">Linux</span>-like environments (e.g.,
<u>[<span style="font-variant: small-caps">Msys2</span>](https://www.msys2.org/)</u>.
For <span style="font-variant: small-caps">Windows</span> users, it is recommended to
use the
<u>[<span style="font-variant: small-caps">GHCup</span>](https://www.haskell.org/ghcup/)</u>
distribution, allow the installer script to deploy
<span style="font-variant: small-caps">Msys2</span> and then install the development
packages.)

# Contact

Please <u>[create an
issue](https://github.com/CubOfJudahsLion/fountain-parser/issues)</u> if
you find a bug.

I can be reached at <span style="font-family: serif">*10951848+CübO̱fJúdãhsLîòn* ă(t)
*users/noreply/gīthụb/cȯm*</span> (without diacritics and replacing
slashes by periods.)

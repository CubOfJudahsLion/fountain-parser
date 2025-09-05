---
title: fountain-parser v.0.1.0.0 README
---

# Synopsis

`fountain-parser` is a small parser library for the
<u>[<span style="font-variant: small-caps">Fountain</span>](https://fountain.io/)</u>
screenplay format, fully supporting 1.1 version
<u>[syntax](https://fountain.io/syntax/)</u> and producing a simple,
easy easy to grok <span style="font-variant: small-caps">AST</span>.

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
<u>[LICENSE](run:./LICENSE)</u> file for more details.

# Motivation

The <u>[*Developers* section](https://fountain.io/developers/)</u> of
the Fountain site provides a link to a <u>[parsing
library](https://github.com/nyousefi/Fountain)</u> in
<span style="font-variant: small-caps">Objective C</span>. This presents a portability
issue: there *are* projects that make it possible to bridge
<span style="font-variant: small-caps">Objective C</span> and
<span style="font-variant: small-caps">Haskell</span>, they’re platform- or
framework-specific. That library informs this project in matching the
different Fountain entities even as it uses different parsing methods.

## Prospective Related Projects

`fountain-parser` aims to power a series of command-line utilities for
conversion from <span style="font-variant: small-caps">Fountain</span> to a series of
convenient formats (like `.TEX`) without intervention from thirds.

## My software already supports Fountain

The <u>[*Apps* section](https://fountain.io/apps/)</u> of the
<span style="font-variant: small-caps">Fountain</span> site lists software that also
imports or exports the format. There’s a caveat: most are either
*cloud-based* and/or *proprietary*. By favoring (mostly) open formats,
*fountain-parse* allows integration into many
<span style="font-variant: small-caps">FLOSS</span> tools, enabling entirely
non-proprietary workflows and helping the creation of compound documents
such as production bibles.

# Implementation Specifics

- As per the [syntax guide](https://fountain.io/syntax/):

  - This library expects Fountain text to be encoded in
    <span style="font-variant: small-caps">UTF-8</span>.

  - Tabs are converted into **four** spaces.

  - Your line-spacing is respected.

  - Initial spaces are ignored everywhere except in action lines.

  - A line with two spaces doesn’t count as an empty line.

- All parsing functions expect `Text` inputs. File I/O is left to the
  application or framework.

- Varying-width <span style="font-variant: small-caps">Unicode</span> spaces are either
  converted into regular spaces or suppressed if they’re hairline- or
  zero-width.

- Vertical tabs and form-feed characters are interpreted as line
  changes. For vertical spacing, use multiple blank lines and/or the
  Fountain form feed character sequence instead.

- The parser keeps everything: notes, boneyards, sections and synopses.
  Some possible conversion targets have equivalents to those, thus it
  might be desirable to preserve them.

## Tentative Grammar

The following is an attempt to formalize the syntax in
[<span style="font-variant: small-caps">ABNF</span>](https://datatracker.ietf.org/doc/html/rfc5234),
drawing from the [syntax guide](https://fountain.io/syntax/) and
<span style="font-variant: small-caps">Objective C</span>
<u>[implementation](https://github.com/nyousefi/Fountain)</u>. It
incorporates <span style="font-variant: small-caps">Unicode</span> codepoints and tries
to err in the side of lenience.

<span style="color: gray">;; The grammar is currently ambiguous,
requiring unrestricted lookahead or backtracking.</span>  
<span style="color: gray">;; The "maximal-munch" rule applies: the
longest match is considered the valid one.</span>  
  
<span style="color: gray">;; Some character will be described as regular
expressions character classes inside prose</span>  
<span style="color: gray">;; values (i.e., \<\[...\]\>) as it's more
concise than enumerating a lot of character ranges.</span>  
<span style="color: gray">;; \uxxx... (unicode character in hex),
\p{...}/\P{...} (having/not-having Unicode</span>  
<span style="color: gray">;; property) and \[:defined-set:\] notations
will be used.</span>  
  
  
*fountain-screenplay* **=**
<span style="color: teal"></span>*empty-line* **\[***cover-page***\]**
*script-content*  
  
*cover-page* **=** <span style="color: teal"></span>*cover-entry*  
  
*cover-entry* **=** *cover-key* <span style="color: BrickRed">":"</span>
<span style="color: teal"></span>*space* *cover-value*  
  
*cover-key* **=**
<span style="color: teal"></span>**\<**<span style="color: olive">\[^:\[:newline-char:\]\]</span>**\>**  <span style="color: gray">;
later trimmed of end spaces</span>  
  
*cover-value* **=** *single-value* **/** *multi-value*  
  
*single-value* **=** <span style="color: teal"></span>*non-newline*  
  
*multi-value* **=** <span style="color: teal"></span>**(***newline*
<span style="color: teal"></span>*space*
<span style="color: teal"></span>*non-newline***)**  
  
*script-content* **=**
<span style="color: teal"></span>**(***section-indicator* **/**
*master-scene* **/** *synopse***)**  
  
*section-indicator* **=**
<span style="color: teal"></span><span style="color: BrickRed">"#"</span>
<span style="color: teal"></span>*space*
<span style="color: teal"></span>*non-newline* *newline* *empty-line*  
  
*master-scene* **=** *master-scene-heading* *scene-content*  
  
*master-scene-heading* **=** *int-ext* *scene-description*
<span style="color: teal"></span>*scene-number*
<span style="color: teal"></span>*space* *newline* *empty-line*  
  
*int-ext* **=** **(**<span style="color: BrickRed">"I"</span>
**(\[**<span style="color: BrickRed">"."</span>**\]**
<span style="color: BrickRed">"/"</span>
**\[**<span style="color: BrickRed">"E"</span>**\]** **/**
**(**<span style="color: BrickRed">"NT"</span>
**\[**<span style="color: BrickRed">"."</span>**\]**
<span style="color: BrickRed">"/EXT"</span>**))** **/**
<span style="color: BrickRed">"E"</span>
**\[**<span style="color: BrickRed">"XT"</span>**\])**
**(**<span style="color: BrickRed">"."</span> **/** *space***)**  
  
*scene-description* **=** <span style="color: teal"></span>*space*
<span style="color: teal"></span>*non-newline-or-hash*  
  
*scene-number* **=** <span style="color: BrickRed">"#"</span>
<span style="color: teal"></span>*scene-number-character*
<span style="color: BrickRed">"#"</span>
<span style="color: teal"></span>*space*  
  
*scene-number-character* **=** *alphanumeric* **/**
<span style="color: BrickRed">"-"</span> **/**
<span style="color: BrickRed">"."</span>  
  
  
*heading* **=**
<span style="color: teal"></span><span style="color: BrickRed">"#"</span>
<span style="color: teal"></span>*space*  
  
*power-action-line* **=** <span style="color: BrickRed">"!"</span>
<span style="color: teal"></span>**\<**<span style="color: olive">\[^!\n\]</span>**\>**
<span style="color: BrickRed">"\n"</span>  
  
*power-character-line* **=** <span style="color: BrickRed">"@"</span>
<span style="color: teal"></span>**\<**<span style="color: olive">\[^\[:newline-char:\](\]</span>**\>**
**\[**<span style="color: BrickRed">"("</span>
**\<**<span style="color: olive">\[^\[:newline-char:\])\]</span>**\>**
<span style="color: BrickRed">")"</span>**\]**
<span style="color: teal"></span>*space*
**\[**<span style="color: BrickRed">"^"</span>
<span style="color: teal"></span>*space***\]**  
  
<span style="color: gray">;power-scene-header-line = </span>  
  
  
  
*vtab* **=** <span style="color: Brown">**%x**0B</span>  
  
*ff* **=** <span style="color: Brown">**%x**0C</span>  
  
*newline* **=** *CR* **\[***LF***\]**  
        **/** *LF* **\[***CR***\]**  
        **/** *vtab*    <span style="color: gray">; We interpret
vertical tabbing as a newline too</span>  
        **/** *ff*      <span style="color: gray">; Same for
form-feeds</span>  
        **/**
<span style="color: Brown">**%x**0085</span>  <span style="color: gray">;
Unicode next-line</span>  
        **/**
<span style="color: Brown">**%x**2028</span>  <span style="color: gray">;
Unicode line-separator</span>  
        **/**
<span style="color: Brown">**%x**2029</span>  <span style="color: gray">;
Unicode paragraph-separator</span>  
        <span style="color: gray">; These are all converted into your
OS's native newline at the end.</span>  
  
*newline-char*  **=** *CR* **/** *LF* **/** *vtab* **/** *ff* **/**
<span style="color: Brown">**%x**0085</span> **/**
<span style="color: Brown">**%x**2028</span> **/**
<span style="color: Brown">**%x**2029</span> <span style="color: gray">;
characters used in the former</span>  
  
*space* **=** *SP*          <span style="color: gray">; normal
space</span>  
      **/** *HTAB*        <span style="color: gray">; tabulator --
converts into 4 spaces</span>  
      **/**
<span style="color: Brown">**%x**00A0</span>      <span style="color: gray">;
non-breaking</span>  
      **/** <span style="color: Brown">**%x**2000**-**2009</span>
<span style="color: gray">; varying-width Em/En-based spaces</span>  
      **/**
<span style="color: Brown">**%x**202F</span>      <span style="color: gray">;
narrow non-breaking</span>  
      **/**
<span style="color: Brown">**%x**205F</span>      <span style="color: gray">;
mathematical middle-space</span>  
      **/**
<span style="color: Brown">**%x**3000</span>      <span style="color: gray">;
Ideographic space</span>  
      <span style="color: gray">; These are turned into one or more
fixed-width spaces (SP); we're trying to imitate</span>  
      <span style="color: gray">; a typewriter.</span>  
      <span style="color: gray">; Hairline or zero-width spaces and
joiners are removed previous to parsing.</span>  
      <span style="color: gray">; Same goes for any control characters
not listed as space or newline.</span>  
  
*non-newline* **=**
**\<**<span style="color: olive">re:\[^\[:newline-char:\]\]</span>**\>**  
  
*non-newline-or-hash* **=**
**\<**<span style="color: olive">re:\[^\[:newline-char:\]#\]</span>**\>**  
  
  

# Building

<span style="font-variant: small-caps">GHC</span> 9.6.7 and
<span style="font-variant: small-caps">Cabal</span> 3.0 (or greater) are required to
compile and run the test suite (once implemented.)

The project uses the `GHC2021` language default. While it might be
possible to compile it in earlier versions than 9.6.7, this default is
only available since 9.2.1, constituting a hard limit.

Some of the included scripts require `make`, `sed` and other similar
utilities usually found in <span style="font-variant: small-caps">Linux</span> or
<span style="font-variant: small-caps">Linux</span>-like environments (e.g.,
<u>[<span style="font-variant: small-caps">MSYS2</span>](https://www.msys2.org/)</u>.)
However, nothing prevents the user from running `cabal`, `pandoc` or
`pdflatex` as shown in the <u>[`Makefile`](run:./Makefile)</u>.

# Contact

Please <u>[create an
issue](https://github.com/CubOfJudahsLion/fountain-parser/issues)</u> if
you find a bug.

I can be reached directly at
<span style="font-family: serif">*10951848+CübO̱fJúdãhsLîòn* ă(t)
*users/noreply/gīthụb/cȯm*</span> (without diacritics and replacing
slashes by periods.)

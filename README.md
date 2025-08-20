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
convenient formats, (`.OTF`, `.TEX`) without intervention from thirds.

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

## Tentative Formal Grammar

The following is an attempt to formalize the syntax in
[<span style="font-variant: small-caps">ABNF</span>](https://datatracker.ietf.org/doc/html/rfc5234),
drawing from the [syntax guide](https://fountain.io/syntax/) and
<span style="font-variant: small-caps">Objective C</span> implementation. It
incorporates <span style="font-variant: small-caps">Unicode</span> codepoints and it
tries to err in the side of lenience.

<span style="color: gray">; The grammar is currently very ambiguous,
requiring unrestricted lookahead or backtracking.</span>  
**fountain-screenplay** = \*empty-line (script-content / cover-page
1\*empty-line script-content)  
**cover-page** = 1\*cover-entry  
**cover-entry** = cover-key \*space <span style="color: red">":"</span>
\*space cover-value  
**cover-key** = <span style="color: red">"TITLE"</span> /
<span style="color: red">"CREDIT"</span> /
<span style="color: red">"AUTHOR"</span> /
<span style="color: red">"SOURCE"</span> /
<span style="color: red">"DRAFT DATE"</span> /
<span style="color: red">"CONTACT"</span>  
**cover-value** = single-value / multi-value  
**single-value** = 1\*non-newline newline  
**multi-value** = newline 1\*indented-sub-value  
**indented-sub-value** = (htab / 2\*8(space)) 1\*non-newline newline  
**script-content** = \*(section-indicator / master-scene / synopse)  
**section-indicator** = 1\*<span style="color: red">"#"</span> \*space
1\*non-newline newline empty-line  
**master-scene** = master-scene-heading scene-content  
**master-scene-heading** = int-ext scene-description \*scene-number
\*space newline empty-line  
**int-ext** = (<span style="color: red">"I"</span>
(\[<span style="color: red">"."</span>\]
<span style="color: red">"/"</span>
\[<span style="color: red">"E"</span>\] /
(<span style="color: red">"NT"</span>
\[<span style="color: red">"."</span>\]
<span style="color: red">"/EXT"</span>)) /
<span style="color: red">"E"</span>
(<span style="color: red">"ST"</span> /
<span style="color: red">"XT"</span>))
(<span style="color: red">"."</span> / space)  
**scene-description** = \*space 1\*non-newline-or-hash  
**scene-number** = <span style="color: red">"#"</span>
1\*scene-number-character <span style="color: red">"#"</span> \*space  
**scene-number-character** = alphanumeric /
<span style="color: red">"-"</span> /
<span style="color: red">"."</span>  
**uppercase-letter**  = <span style="color: red">%x41-5A</span>  
**digit** = <span style="color: red">"0"</span> /
<span style="color: red">"1"</span> /
<span style="color: red">"2"</span> /
<span style="color: red">"3"</span> /
<span style="color: red">"4"</span> /
<span style="color: red">"5"</span> /
<span style="color: red">"6"</span> /
<span style="color: red">"7"</span> /
<span style="color: red">"8"</span> /
<span style="color: red">"9"</span>  
**alphanumeric** = alpha / digit  
**space** = <span style="color: red">" "</span>  
      /
<span style="color: red">%x00A0</span>      <span style="color: gray">;
non-breaking</span>  
      / <span style="color: red">%x2000-2009</span>
<span style="color: gray">; varying-width Em/En-based spaces</span>  
      /
<span style="color: red">%x202F</span>      <span style="color: gray">;
narrow non-breaking</span>  
      /
<span style="color: red">%x205F</span>      <span style="color: gray">;
mathematical middle-space</span>  
      /
<span style="color: red">%x3000</span>)     <span style="color: gray">;
Ideographic space</span>  
      <span style="color: gray">; These are turned into the regular
space; we’re trying to imitate a typewriter.</span>  
      <span style="color: gray">; Hairline or zero-width spaces and
joiners are pre-filtered out previous to parsing.</span>  
      <span style="color: gray">; Same goes for control
characters.</span>  
**newline** = cr \[lf\]  
        / lf \[cr\]  
        /
<span style="color: red">%x0B</span>    <span style="color: gray">; We
interpret vertical tabbing as a newline too</span>  
        /
<span style="color: red">%x0C</span>    <span style="color: gray">; As
well as form-feeding</span>  
        /
<span style="color: red">%x0085</span>  <span style="color: gray">;
Unicode next-line</span>  
        /
<span style="color: red">%x2028</span>  <span style="color: gray">;
Unicode line-separator</span>  
        /
<span style="color: red">%x2029</span>  <span style="color: gray">;
Unicode paragraph-separator</span>  
        <span style="color: gray">; These are all converted into the
POSIX newline "\n".</span>  
**empty-line** = \*space newline  
**character** = \<any Unicode character\>  
**non-newline** = \<character - newline\>  
**non-newline-or-hash** = \<non-newline -
<span style="color: red">"#"</span>\>  

# Building

<span style="font-variant: small-caps">GHC</span> 9.6.7 and
<span style="font-variant: small-caps">Cabal</span> 3.0 (or greater) are required to
compile and run the test suite (once implemented.)

The project uses the `GHC2021` language default. While it might be
possible to compile it in earlier versions than 9.6.7, this default is
only available since 9.2.1., so that constitutes a hard version limit
for those who want to try other versions.

Some of the included scripts require `make`, `awk` and other similar
utilities usually found in <span style="font-variant: small-caps">Linux</span> or
<span style="font-variant: small-caps">Linux</span>-like environments (e.g.,
<u>[<span style="font-variant: small-caps">MSYS2</span>](https://www.msys2.org/)</u>.)
However, nothing prevents the user from running `cabal`, `pandoc` or
`pdflatex` as shown in the <u>[`Makefile`](run:./Makefile)</u>.

# Contact

Please <u>[create an
issue](https://github.com/CubOfJudahsLion/fountain-parser/issues)</u> if
you find a bug.

I can be reached directly at *10951848+CübO̱fJúdãhsLîòn* ă(t)
*users/noreply/gīthụb/cȯm* (without accents and replacing slashes by
periods.)

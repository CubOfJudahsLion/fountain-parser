`fountain-parser` is a small parser library for the
[[Fountain](https://fountain.io/)]{.underline} screenplay format,
supporting 1.1 version
[[syntax](https://fountain.io/syntax/)]{.underline} and producing a
simple, easy to grok [AST]{.sans-serif}.

`fountain-parser` is written in
[[Haskell](https://haskell.org)]{.underline} and it uses the
[[Megaparsec](https://hackage.haskell.org/package/megaparsec)]{.underline}
library for parsing.

# Motivation

The "*Developers*" section of the Fountain site provides a link to a
[[parsing library](https://github.com/nyousefi/Fountain)]{.underline} in
[Objective C]{.sans-serif}. This already presents a portability
issue:there *are* projects that make it possible to connect Objective C
to Haskell, but they're either platform- or framework-specific. It also
employs a multi-pass stategy where every stage creates a modified
version of the source, and it's heavily reliant on *Regular
Expressions*.

Thus, to create a light-weight, performant and portable solution, it's
necessary to start from scratch.

`fountain-parser` aims to power a series of command-line utilities for
conversion from Fountain to a series of convenient formats, such as
`.OTF` or `.PDF`, without the intervention of thirds.

# Implementation Specifics

Fountain files are [UTF-8]{.sans-serif} text files. While this library
doesn't impose any file naming scheme, it is customary for Fountain
files to have the extension `.fountain`, `.txt` or `.spmd` (meaning
"**S**creen**p**lay **M**ark**d**own", one of the formats that
eventually merged into Fountain.)

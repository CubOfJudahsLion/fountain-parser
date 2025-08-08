# Synopsis

`fountain-parser` is a small parser library for the
[Fountain](https://fountain.io/) screenplay
format, fully supporting 1.1 version
[syntax](https://fountain.io/syntax/) and producing a
simple, easy to grok AST.

`fountain-parser` is written in
[Haskell](https://haskell.org) and it uses
the
[Megaparsec](https://hackage.haskell.org/package/megaparsec)
library for parsing.

# Status

Currently, this is *pre-alpha* software, not yet usable in any form.
We'll have something testable soon enough.

# Motivation

The "*Developers*" section of the Fountain site provides a link to a
[parsing library](https://github.com/nyousefi/Fountain) in
Objective C. This already presents a portability issue:
there *are* projects that make it possible to connect Objective C to
Haskell, but they're either platform- or framework-specific. It also
employs a multi-pass stategy where every stage creates a modified
version of the source, and it's heavily reliant on *Regular
Expressions*.

Thus, to create a light-weight, performant and portable solution, it's
necessary to start from scratch.

`fountain-parser` aims to power a series of command-line utilities for
conversion from Fountain to a series of convenient formats, such as
`.OTF`, `.TEX` or `.PDF`, without the intervention of thirds.

# Implementation Specifics

Fountain files are UTF-8 text files. While this library
doesn't impose any file naming scheme, it is customary for Fountain
files to have the extension `.fountain`, `.txt` or `.spmd` (meaning
"**S**creen**p**lay **M**ark**d**own", the format that eventually became
Fountain.)

## But my software already supports Fountain!

The ["*Apps*" section](https://fountain.io/apps/) of the
Fountain site lists a number of apps that can import or export the
format. The caveat: most are either cloud-based and/or proprietary. By
favoring (mostly) open formats, *fountain-parse* allows integration into
many FLOSS tools, helping the creation of compound documents (such as
production bibles) and entirely non-proprietary workflows.

# Contact

Please create an issue if you find one.

I can be reached directly at 10951848+CübÔfJúdãhsLîòn at
users/noreply/gîthũb/cöm (without accents and replacing
slashes by periods.)

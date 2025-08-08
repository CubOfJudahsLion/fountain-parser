# Synopsis

`fountain-parser` is a small parser library for the
<u>[<span class="sans-serif">Fountain</span>](https://fountain.io/)</u>
screenplay format, fully supporting 1.1 version
<u>[syntax](https://fountain.io/syntax/)</u> and producing a simple,
easy to grok <span class="sans-serif">AST</span>.

`fountain-parser` is written in
<u>[<span class="sans-serif">Haskell</span>](https://haskell.org)</u>
and it uses the
<u>[<span class="sans-serif">Megaparsec</span>](https://hackage.haskell.org/package/megaparsec)</u>
library for parsing.

# Status

Currently, this is *pre-alpha* software, not yet usable in any form.
We’ll have something testable soon enough.

# Motivation

The “*Developers*” section of the Fountain site provides a link to a
<u>[parsing library](https://github.com/nyousefi/Fountain)</u> in
<span class="sans-serif">Objective C</span>. This already presents a
portability issue: there *are* projects that make it possible to connect
Objective C to Haskell, but they’re either platform- or
framework-specific. It also employs a multi-pass stategy where every
stage creates a modified version of the source, and it’s heavily reliant
on *Regular Expressions*.

Thus, to create a light-weight, performant and portable solution, it’s
necessary to start from scratch.

`fountain-parser` aims to power a series of command-line utilities for
conversion from Fountain to a series of convenient formats, such as
`.OTF`, `.TEX` or `.PDF`, without the intervention of thirds.

# Implementation Specifics

Fountain files are <span class="sans-serif">UTF-8</span> text files.
While this library doesn’t impose any file naming scheme, it is
customary for Fountain files to have the extension `.fountain`, `.txt`
or `.spmd` (meaning “**S**creen**p**lay **M**ark**d**own”, the format
that eventually became Fountain.)

## But my software already supports Fountain!

The <u>[“*Apps*” section](https://fountain.io/apps/)</u> of the Fountain
site lists a number of apps that can import or export the format. The
caveat: most are either cloud-based and/or proprietary. By favoring
(mostly) open formats, *fountain-parse* allows integration into many
FLOSS tools, helping the creation of compound documents (such as
production bibles) and entirely non-proprietary workflows.

# Contact

Please create an issue if you find one.

I can be reached directly at
*<span class="sans-serif">10951848+CübÔfJúdãhsLîòn</span> at
<span class="sans-serif">users/noreply/gîthũb/cöm</span>* (without
accents and replacing slashes by periods.)

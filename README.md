# Open Geneva

*Geneva* is the portable document preparation system. It follows the
principle *Write once, read anywhere*. Geneva enables the creation and
archival of truly portable documents by isolating document content and
structure from document presentation.

Geneva achieves this by introducing a generic but well defined document
structure independent from its stored representation. In addition to
classic document features such as paragraphs, listings, sections, tables
and text markup, Geneva defines a generic *media type* for embedding
arbitrary content and enable extensibility in a plug-in oriented way.

*Open Geneva* is the reference implementation of Geneva written in
*Common Lisp*. It implements Geneva and defines a diverse toolchain
composed of input interfaces and presentation backends. Its input
interfaces, such as the plain text oriented *Mk2 markup language* and the
Geneva API, are user and programmer facing frontends to document
authoring. Its presentation backends render documents to several targets
including web, print and plain text media.


## Documentation

 * [Geneva Document Specification](geneva-document.html)
 * [Open Geneva Manual](open-geneva.html)
 * [The Mk2 Markup Language](mk2.html)

## Dependencies

 * [mpc](https://github.com/eugeneia/mpc)
 * [file-types](https://github.com/eugeneia/file-types)
 * [macro-html](https://github.com/eugeneia/macro-html)
 * [trivial-documentation](https://github.com/eugeneia/trivial-documentation)
 * [texp](https://github.com/eugeneia/texp)

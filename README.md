ipynb
=====

[![CI
tests](https://github.com/jgm/ipynb/workflows/CI%20tests/badge.svg)](https://github.com/jgm/ipynb/actions)

ipynb is a Haskell library for representing [Jupyter notebooks]
and converting them to and from the JSON .ipynb format.
The format is documented here:
<https://nbformat.readthedocs.io/en/latest/>.

Both version 4 and version 3 are supported, though the
data structure used is the same.  A phantom type is used to
provide different JSON serializations for the two versions.
When working with version 4, use `Notebook NbV4`; when
working with version 3, use `Notebook NbV3`.

The test suite verifies that `write . read` and `read . write`
are identities, up to semantically insignificant differences
in the JSON output.

[Jupyter notebooks]: https://jupyter.org

Related work
------------

The [JuPyTer-notebook] library has a similar job description,
but there are some important differences:

- JuPyTer-notebook parses code cell output as JSON;
  ipynb has a custom representation of Output.

- JuPyTer-notebook allows representing some illegal
  cells (e.g., with `cell_type` `markdown` but
  also with `execution_count` and `outputs`). ipynb
  goes further in making illegal notebooks unrepresentable.

- ipynb supports both v3 and v4 of nbformat, both in reading
  and writing; JuPyTer-notebook supports only v4 (and does
  not support the more recent v4 feature, `attachments`).

- JuPyTer-notebook parses some notebook metadata into
  custom structures (using data type disjunction (`:|:`)
  to provide a fallback of text or a JSON value when
  parsing fails).  ipynb just leaves all metadata as
  JSON `Value`s.

- JuPyTer-notebook depends on `json-autotype`, and thus has
  a much heavier transitive dependency graph than ipynb.

[JuPyTer-notebook]: http://hackage.haskell.org/package/JuPyTer-notebook


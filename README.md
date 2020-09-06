# Compdoc - Composite Pandoc Format

Compdoc is an alternative format for reading pandoc markdown data using [composite](https://hackage.haskell.org/package/composite-base) and [composite-aeson](https://hackage.haskell.org/package/composite-aeson).

Compdoc will read a pandoc markdown file according to the `JsonFormat` that you
supply to parse the metadata, and place the main body in an `FContents` fields
that can be accessed with the lens `view fContent`.

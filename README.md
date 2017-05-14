# Build

To build:

```
stack build
```

# Usage

Converting a csv into a csv to be imported into Anki.
```
stack-exec vocab.csv fields.txt converters_dir output_dir
```
Adding vocab from a list, or a hypothes.is export.
```
stack-exec add-vocab-exe language vocab.csv hypothesis.csv converter.txt
```

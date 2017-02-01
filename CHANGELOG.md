# Change Log

## 0.2.0 - 2017-01-31

### Added

- The "plaintext" option for documents. Having the "plaintext: ..." header in the document will tell Saha to bypass the CommonMark conversion and insert the original plaintext directly to the template's `\content\`.

### Fixed

- Unix sockets created by Saha did not always have the correct file permission mode. Fixed it to always have an accessible file mode.

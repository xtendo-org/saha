# Change Log

## Unreleased

### Added

- Special header `redirect` which allows redirect response (HTTP 301 Moved).

### Fixed

- Correct Content-Type header for GIF and TXT files.
- Apply completely static build: Change the Stackage curation to LTS. Build with `stack build --docker`.

## 0.2.1 - 2017-05-11

### Changed

- Any file extension in static file serving is now supported.
- Tilde (~) is now allowed in URLs.

## 0.2.0 - 2017-01-31

### Added

- The "plaintext" option for documents. Having the "plaintext: ..." header in the document will tell Saha to bypass the CommonMark conversion and insert the original plaintext directly to the template's `\content\`.

### Fixed

- Unix sockets created by Saha did not always have the correct file permission mode. Fixed it to always have an accessible file mode.

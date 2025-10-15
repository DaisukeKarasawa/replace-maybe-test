# Changelog

All notable changes to this project will be documented in this file.

## 0.2.1
- Improve error clarity: `sum --strict` now uses `Either` to report the first invalid token; `email` distinguishes `user-not-found` vs `email-missing`.

## 0.2.0
- Replace examples demo CLI with practical subcommands: `greet`, `sum`, `head`, `email` (Breaking change)
- Internally use Maybe in multiple places within normal command flows

## 0.1.0
- Initial release.

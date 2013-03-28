Finite state transducers
========================

A Haskell package for construction and running of finite state
transducers, as based on the concepts of transducers and regular
relations developed by Xerox.  The syntax of Xerox's fst program has
functioned as an inspiration for the syntax of fstStudio.

The application was written purely in Haskell, and is intended to be a
tool for the Haskell programmer, especially for ones that develop
language applications.

This repository covers versions 0.10.0.0+ and was started when the
original version of the package (0.9.0.1) was revived by
[Baldur Blöndal](https://github.com/Icelandjack) and
[John J. Camilleri](https://github.com/johnjcamilleri).

## Release

When uploading a release to Hackage, take the following steps:

1. Choose a version number `1.2.3.4` and update it in `fst.cabal` in these places:
  1. The `version:` field
  1. The `source-repository this` section
1. Test everything with `cabal clean ; cabal configure --enable-tests ; cabal haddock ; cabal build ; cabal sdist`
1. If everything works, check the package using Hackage's [package tester](http://hackage.haskell.org/packages/upload.html). If you change anything, start again from step 2.
1. Commit everything to the repository, with commit message `"Release 1.2.3.4"`
1. Tag the release in the repository: `git tag -a v1.2.3.4 -m "Release 1.2.3.4"`. **Note** the `v` in the tag name.
1. Push to GitHub with `git push --tags`
1. Upload to Hackage via the [upload page](http://hackage.haskell.org/packages/upload.html).


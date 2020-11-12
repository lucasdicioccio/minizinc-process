# Revision history for minizinc-process

## Unreleased changes

* Improve TemplateHaskell to arrays of floats and arrays of bools.

## 0.1.4.0 -- 2020-11-12

* Add primitive TemplateHaskell generation of interface types.

## 0.1.3.0 -- 2020-11-11

* Unify backing code when getting last result and streaming results (better RAM usage for long sequences).
* Add function to clean temporary files.
* Initial set of unit tests.
* Use bracket and cleanupProcess to dispose of resources.

## 0.1.2.1 -- 2020-11-08

* Handle case where final result is partial.

## 0.1.2.0 -- 2020-11-08

* Pass a parsed JSON Value rather than the Value in ResultHandler.

## 0.1.1.0 -- 2020-11-07

* Add streaming results API.
* Add helper to set extra arguments.

## 0.1.0.0 -- YYYY-mm-dd

* Initial version.

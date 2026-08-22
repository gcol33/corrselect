## Submission

corrselect 3.3.0, an update of the CRAN version 3.2.3 (published 2026-07-18).

The release fixes a set of correctness bugs in `modelPrune()`, `corrPrune()` and the
C++ enumeration backend, and changes one documented behaviour: numeric-categorical
pairs are now thresholded on eta (the correlation ratio) rather than eta-squared, so
every pair type in `assocSelect()` and `corrPrune()` is compared on the same scale.
Mixed-type results can therefore differ from 3.2.3. This is recorded under "Breaking
Changes" in NEWS.md.

## R CMD check results

0 errors | 0 warnings | 0 notes

## Test environments

* local: Windows 11 x64, R 4.6.0 -- Status: OK
* win-builder: R-devel (2026-08-21 r90440) and R 4.6.1 -- both Status: OK

## Notes for the reviewer

* The single `\dontrun{}` block (in `?modelPrune`) demonstrates a user-supplied
  custom engine built on INLA, which is not distributed through CRAN. The example
  cannot be executed on the CRAN build farm. Every other example in the package runs
  unwrapped.

## Downstream dependencies

There are no reverse dependencies.

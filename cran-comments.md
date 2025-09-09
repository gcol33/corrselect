## Test environments
- Local Windows 11, R 4.5.1
- win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs.
There were 3 NOTEs:

* New submission: expected for initial release.
* Future file timestamps: "unable to verify current time". This is a Windows artifact and not an issue.
* Top-level files: "README.md or NEWS.md cannot be checked without pandoc". CRAN has pandoc, so this will not be a problem.

## Downstream dependencies
There are currently no downstream dependencies.

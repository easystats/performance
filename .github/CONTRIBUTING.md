# Contributing to insight

This outlines how to propose a change to **insight**. 

## Fixing typos

Small typos or grammatical errors in documentation may be edited directly using the GitHub web interface, so long as the changes are made in the _source_ file. If you want to fix typos in the documentation, please edit the related `.R` file in the `R/` folder. Do _not_ edit an `.Rd` file in `man/`.

## Filing an issue

The easiest way to propose a change or new feature is to file an issue. If you've found a
bug, you may also create an associated issue. If possible, try to illustrate your proposal or the bug with a minimal [reproducible example](https://www.tidyverse.org/help/#reprex).

## Pull requests

*  Please create a Git branch for each pull request (PR).
*  Your contributed code should roughly follow the [R style guide](http://style.tidyverse.org), but in particular our [**easystats convention of code-style**](https://github.com/easystats/easystats#convention-of-code-style).
*  insight uses [roxygen2](https://cran.r-project.org/package=roxygen2), with
[Markdown syntax](https://cran.r-project.org/web/packages/roxygen2/vignettes/markdown.html),
for documentation.
*  insight uses [testthat](https://cran.r-project.org/package=testthat). Adding tests to the PR makes it easier for me to merge your PR into the code base.
*  If your PR is a user-visible change, you may add a bullet to the top of `NEWS.md` describing the changes made. You may optionally add your GitHub username, and links to relevant issue(s)/PR(s).

## Code of Conduct

Please note that this project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree to
abide by its terms.

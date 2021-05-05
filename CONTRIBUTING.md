# Contributing

We love pull requests from everyone in the lab. By participating in this project, you agree to abide by our [code of conduct](CONDUCT.md) and you are supporting your lab mates!

## Getting Started

* Make sure you have a [GitHub account](https://github.com/signup/free). If you are not familar with git and GitHub, take a look at <http://happygitwithr.com/> to get started.
* [Submit a post for your issue](https://github.com/jepa/Rcoru/issues/), assuming one does not already exist.
  * Clearly describe your issue, including steps to reproduce when it is a bug, or some justification for a proposed improvement.
* [Fork](https://github.com/jepa/Rcoru/#fork-destination-box) the repository on GitHub to make a copy of the repository on your account. Or use this line in your shell terminal:

    `git clone git@github.com:your-username/rrtools.git`
    
## Making changes

* Edit the files, save often, and make commits of logical units, where each commit indicates one concept
* Follow our [style guide](http://adv-r.had.co.nz/Style.html).
* Make sure you write [good commit messages](http://tbaggery.com/2008/04/19/a-note-about-git-commit-messages.html).
* Make sure you have added the necessary tests for your code changes.
* Run _all_ the tests using `devtools::check()` to assure nothing else was accidentally broken.
* If you need help or unsure about anything, post an update to [your issue](https://github.com/benmarwick/rrtools/issues/).

## Submitting your changes

Push to your fork and [submit a pull request](https://github.com/benmarwick/rrtools/compare/).

At this point you're waiting on us. We like to at least comment on pull requests
within a few days (and, typically, one business day). We may suggest
some changes or improvements or alternatives.

Some things you can do that will increase the chance that your pull request is accepted:

* Engage in discussion on [your issue](https://github.com/benmarwick/rrtools/issues/).
* Be familiar with the background literature cited in the [README](README.Rmd)
* Write tests that pass `devtools::check()`.
* Follow our [code style guide](http://adv-r.had.co.nz/Style.html).
* Write a [good commit message](http://tbaggery.com/2008/04/19/a-note-about-git-commit-messages.html).

## Creating new functions

There are multiple waws to create and document functions. We like the tutorials provided by [Fong Chun Chan's Blog](https://tinyheero.github.io/jekyll/update/2015/07/26/making-your-first-R-package.html) and [Rpackages](https://r-pkgs.org/man.html). Regardless of what method you follow, each function you create should have _at least_ the following documentation:

- `#' @author`
- `#' @export` 
- `#' @param` 
- `#' @return`
- `#' examples`

Remember to ask for colagues to test the function and "try to brake it".

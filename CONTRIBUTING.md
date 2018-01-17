## Guidelines for Contributing

Thanks for checking out our project! If you haven't already, please check out the [readme](README.md) for general info about this project.

## Contributor Code of Conduct
All contributors will be expected to follow our [code of conduct](CODE_OF_CONDUCT.md).

## For the General Public
If you're not a member of the Weecology lab, we ask that you use one of the following two methods for contributing:

1. Create an issue -- if you spot any typos, bugs, or have general suggestions, etc. You can also use this to participate in ongoing discussions. For more info, please check out this Github [guide](https://guides.github.com/features/issues/).

2. Fork and create a pull request -- if you have suggested bugfixes or changes. For more info, please check out this Github [guide](https://help.github.com/articles/about-pull-requests/). We ask that you follow our guidelines below on documentation and testing.

### Weecologists

If you're actively working on this repo, then you should have write access to create branches for any new features or bugfixes. Please see the lab-wiki for info on using branches in a shared repository. 

If you don't have write access and you would like to, please contact @gmyenni for access.

### Documentation

If you are contributing code to this project, you generally don't need any additional packages, since the documentation will be written as comments in the R scripts. If you are also building the package, see the [section below](#building) for more details.

In most cases, you'll be creating a new function and then documenting it. You can check the existing functions for examples, but here's a basic template:
```
#' @title {this is the heading for the help file}
#'
#' @description {A description of the function}
#'
#' @param {name of a function argument} {what the argument does}
#' @return {what is returned from the function}
#' @examples
#' {R code that is an example use of the function}
#' @export
#'
newfunc <- function() ...
```

Note that you can also include links to other functions, math formatting, and more. For more details, see the [chapter on documentation ](http://r-pkgs.had.co.nz/man.html) in Hadley Wickham's book for R packages.

### Testing

### Building
However, you will need both the `roxygen2` and the `devtools` package to build the documentation. For more details, see the [section below](#building).
# tableHTML

#### Released Version

[![CRAN version](http://www.r-pkg.org/badges/version/tableHTML)](https://cran.r-project.org/package=tableHTML)

#### Build Status

[![Travis-CI Build Status](https://travis-ci.org/LyzandeR/tableHTML.svg?branch=master)](https://travis-ci.org/LyzandeR/tableHTML)

#### Description

This is a package for creating easily CSS-ible HTML tables, which are compatible with R shiny.

## Installation

To install the latest released version from CRAN you just need to run on your console:

```r
install.packages('tableHTML')
```

To install the development version you need to have the `devtools` package installed. To install devtools type in your console: `install.packages('devtools')`.

Then to install tableHTML run the following on your console:

```R
devtools::install_github('lyzander/tableHTML')
```

## Usage

By typing on your console:

```R
library(tableHTML)
tableHTML(mtcars)
```

you can see a first example of an HTML table!

## Links - Cran / Tutorial / Examples

To find out about all the functions and how to use the package with shiny you can visit the [online tutorial](https://cran.r-project.org/package=tableHTML/vignettes/tableHTML.html).

To see a gallery of examples you can visit the [examples page](https://cran.r-project.org/web/packages/tableHTML/vignettes/examples.html).

For a tutorial on how to build a css file and how to use that in shiny (in a fast way) you can visit the [make_css and shiny](https://cran.r-project.org/package=tableHTML/vignettes/make_css.html) link. 

To see the released version you can visit [CRAN](https://cran.r-project.org/package=tableHTML).

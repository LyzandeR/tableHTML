# tableHTML

[![CRAN version](http://www.r-pkg.org/badges/version/tableHTML)](https://cran.r-project.org/package=tableHTML)
[![Travis-CI Build Status](https://travis-ci.org/LyzandeR/tableHTML.svg?branch=master)](https://travis-ci.org/LyzandeR/tableHTML)
[![Coverage Status](https://codecov.io/gh/lyzander/tableHTML/branch/master/graph/badge.svg)](https://codecov.io/gh/lyzander/tableHTML?branch=master)

## Overview

The goal of `tableHTML` is to create easily CSS-ible HTML tables. It is compatible with any application that uses HTML / CSS and has successfully been tested with:

* Shiny
* Rmarkdown
* Microsoft Office

The package follows the `tidyverse` philosophy and uses the pipe operator `%>%` to chain functions together. Apart from applying standard CSS to columns / rows, the package offers the ability to add CSS conditionally (e.g. like column colour ranks) and to create CSS files for shiny.

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

To see a gallery of examples you can visit the [examples page](https://cran.r-project.org/package=tableHTML/vignettes/examples.html).

For a tutorial on how to build a css file and how to use that in shiny (in a fast way) you can visit the [make_css and shiny](https://cran.r-project.org/package=tableHTML/vignettes/make_css.html) link. 

To see the released version you can visit [CRAN](https://cran.r-project.org/package=tableHTML).

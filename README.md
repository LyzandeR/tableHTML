# tableHTML

#### Build Status

[![Travis-CI Build Status](https://travis-ci.org/LyzandeR/tableHTML.svg?branch=master)](https://travis-ci.org/LyzandeR/tableHTML)

#### Description

This is a package for creating easily CSS-ible HTML tables, which are compatible with R shiny.

## Installation

To install the package you need to have the `devtools` package installed. To install devtools type in your console: `install.packages('devtools')`.

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

To find out about all the functions, how to use the package with shiny and see a lot of examples visit the [online documentation](https://lyzander.github.io/tableHTML/).
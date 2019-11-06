## Submiting new version 2.0.0 for tableHTML 

## Test environments
* Windows 10, R 3.5.0, R 3.4.2, R devel (also winbuilder on CRAN)
* MAC OSX
* ubuntu 12.04 LTS (on travis-ci), R 3.5.0

## R CMD check results 
There were no ERRORs, WARNINGs or NOTEs (using qpdf)

## Win_build on CRAN
There was one NOTE about the size of the package being over 5MB (it is 6MB). This 
is because of the vignettes we have created. Because we need to demonstrate how
the package works, we generate a lot of HTML tables which make the HTML files of
the vignettes bigger than usual.

## RevDep

There were no errors, warnings or notes for the reverse dependency gWQS

### gWQS (1.1.1)
Maintainer: Stefano Renzetti <stefano.renzetti88@gmail.com>

0 errors | 0 warnings | 0 notes


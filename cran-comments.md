## Submiting new version 2.1.0 for tableHTML 

## Test environments
* Windows 10, R 4.0.4, R devel (also winbuilder on CRAN)
* MAC OSX
* ubuntu 14.04 LTS (on travis-ci)

## R CMD check results 
There were no ERRORs, WARNINGs or NOTEs (using qpdf)

## Win_build on CRAN
There was one NOTE about the size of the package being over 5MB (it is 6MB). This 
is because of the vignettes we have created. Because we need to demonstrate how
the package works, we generate a lot of HTML tables which make the HTML files of
the vignettes bigger than usual.

## RevDep

There were no errors, warnings or notes for the reverse suggest implicitMeasures



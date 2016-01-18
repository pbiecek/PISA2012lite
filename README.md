Programme for International Student Assessment 2012
===================================================

[![DOI](https://zenodo.org/badge/13191/pbiecek/PISA2012lite.svg)](http://dx.doi.org/10.5281/zenodo.17866)

This is an R package that aggregates data from PISA 2012.
Be warned that the zip is about 300MB.

From wikipedia: The Programme for International Student Assessment (PISA) is a worldwide study by the Organisation for Economic Co-operation and Development (OECD) in member and non-member nations of 15-year-old school pupils' scholastic performance on mathematics, science, and reading. It was first performed in 2000 and then repeated every three years. It is done with view to improving education policies and outcomes. The data has increasingly been used both to assess the impact of education quality on incomes and growth and for understanding what causes differences in achievement across nations. see [http://en.wikipedia.org/wiki/Programme_for_International_Student_Assessment]

The original dataset is avaliable here [http://pisa2012.acer.edu.au/downloads.php]

You may be also interested in R packages PISA2000, PISA2003, PISA2006, PISA2009 with data from othere PISA releases.


To get started, install the latest version of **PISA2012lite** from GitHub:
```{Ruby}
if (!require(devtools)) {
    install.packages("devtools")
    require(devtools)
}
install_github("pbiecek/PISA2012lite")
# load it
library("PISA2012lite")
```
Make sure you have [rtools](http://cran.r-project.org/bin/windows/Rtools/) installed on your computer.

The examples how to use PISA2012lite package are avaliable here ....


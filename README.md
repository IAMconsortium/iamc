# R IAMC package

## Purpose and Functionality

The iamc R package is a collection of R tools provided by the Integrated Assessment Modeling Consortium (IAMC) for data analysis and diagnostics. It can be used to make sure that a data set is in line with rules set by a given project. This rules can be for instance a given naming convention or unit conventions for variables, but also qualitative measures such as that certain variables lie between certain bounds. Besides that the data can be compared to given validation data.

## Installation

For installation an additional repository has to be added in R:

```r
options(repos = c(CRAN = "@CRAN@", rd3mod_repo = "http://www.pik-potsdam.de/rd3mod/R/"))
```
The additional repository can be made availably permanently by adding the line above to a file called `.Rprofile` stored in the home folder of your system (`Sys.glob("~")` in R returns the home directory).

After that the package can be installed using `install.packages`:

```r 
install.packages("iamc")
```

Package updates can be installed using `update.packages` (make sure that the additional repository has been added before running that command):

```r 
update.packages()
```

## Tutorial

The package comes with a vignette describing the basic functionality of the package and how to use it. You can load it with the following command (the package needs to be installed):

```r 
vignette("iamc")
```

## Questions / Problems

In case of questions / problems please contact Jan Dietrich <dietrich@pik-potsdam.de> or take a look at the short FAQ below

## FAQ
Q: Pdflatex reports an error if I want to generate my output PDF. Why? 
A: Please check if the automatic installation of styles is allowed for pdflatex.
If not, the pdf creation process may have been stopped by un-installed style packages (e.g. packageXY.sty). 

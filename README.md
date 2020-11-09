# IAMC Tools

R package **iamc**, version **0.28.0**

  

## Purpose and Functionality

A collection of R tools provided by the Integrated Assessment Modeling Consortium (IAMC) for data analysis and diagnostics. 


## Installation

For installation of the most recent package version an additional repository has to be added in R:

```r
options(repos = c(CRAN = "@CRAN@", pik = "https://rse.pik-potsdam.de/r/packages"))
```
The additional repository can be made available permanently by adding the line above to a file called `.Rprofile` stored in the home folder of your system (`Sys.glob("~")` in R returns the home directory).

After that the most recent version of the package can be installed using `install.packages`:

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
vignette("iamc") # Using and adding IAMC data checks
```

## Questions / Problems

In case of questions / problems please contact Jan Philipp Dietrich <dietrich@pik-potsdam.de>.

## Citation

To cite package **iamc** in publications use:

Dietrich J, Baumstark L, Auer C (2020). _iamc: IAMC Tools_. R package version 0.28.0.

A BibTeX entry for LaTeX users is

 ```latex
@Manual{,
  title = {iamc: IAMC Tools},
  author = {Jan Philipp Dietrich and Lavinia Baumstark and Cornelia Auer},
  year = {2020},
  note = {R package version 0.28.0},
}
```


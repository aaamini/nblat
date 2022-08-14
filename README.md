# nblat
Computes nbhd lattice decompositions

# Install
Use the following command in R to install the package
```r
 devtools::install_github("aaamini/nblat/nblat_package")
```
Then run the examples `Example7.R` and `Example9.R`.

The output of the for loop from `Example7.R` should be something like this:
```r
{2,6,12} -- {1,2,4,5,6,7,8,9,10,11,12,13,14,15}(2048 sets)
{2,6,12} -- {1,2,4,5,6,7,8,9,10,11,12,13,14,15}(2048 sets)
{6,12} -- {4,5,6,7,10,11,12,14,15}(128 sets)
{} -- {7,10}  (4 sets)
{9,12,14} -- {4,5,7,8,9,10,11,12,13,14,15}(256 sets)
```

# Running the tests
Open the project file `nblat.Rproj` for the R package in Rstudio. Then run
```r
 devtools::test()
```

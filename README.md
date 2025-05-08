
# RC.SJ.package

RC.SJ.package is an R package that helps researchers discover and
retrieve datasets from the [Dryad](https://datadryad.org/) data
repository. It supports keyword-based searching, filtering based on
method content, and downloading multiple datasets in bulk.

## Installation

To install the development version of `RC.SJ.package` from your local
machine:

``` r
# Install the devtools package if needed
install.packages("devtools")

# For developers working inside the package directory
devtools::install()

# For package users
devtools::install_github("RuzhuoC/RC.SJ.package")
```

## Example Usage

Here’s an example that searches for Dryad datasets mentioning “climate”
and “butterfly”, filters for experiments or heatwaves, and downloads the
datasets.

``` r
library(RC.SJ.package)

# Search Dryad for relevant datasets
results <- search_datasets(c("climate", "butterfly"), max_results = 50)

# Filter for studies using "experiment" or mentioning "heatwave"
filtered <- search_method(results, c("experiment", "heatwave"))

# Download the matching datasets
download_dataset(filtered)
```

For a full workflow, see the vignette:

``` r
vignette("dryad-workflow")
```

# Example Bioclimatic Data for Ecological Modeling

A simulated dataset with the 19 WorldClim bioclimatic variables
(https://www.worldclim.org/data/bioclim.html) measured at 100 geographic
locations, with species richness as the response variable. Variables are
organized into correlated blocks representing temperature (BIO1-BIO11)
and precipitation (BIO12-BIO19).

## Usage

``` r
bioclim_example
```

## Format

A data frame with 100 rows and 20 variables:

- species_richness:

  Integer. Number of species observed (response variable)

- BIO1:

  Numeric. Annual Mean Temperature

- BIO2:

  Numeric. Mean Diurnal Range

- BIO3:

  Numeric. Isothermality

- BIO4:

  Numeric. Temperature Seasonality

- BIO5:

  Numeric. Max Temperature of Warmest Month

- BIO6:

  Numeric. Min Temperature of Coldest Month

- BIO7:

  Numeric. Temperature Annual Range

- BIO8:

  Numeric. Mean Temperature of Wettest Quarter

- BIO9:

  Numeric. Mean Temperature of Driest Quarter

- BIO10:

  Numeric. Mean Temperature of Warmest Quarter

- BIO11:

  Numeric. Mean Temperature of Coldest Quarter

- BIO12:

  Numeric. Annual Precipitation

- BIO13:

  Numeric. Precipitation of Wettest Month

- BIO14:

  Numeric. Precipitation of Driest Month

- BIO15:

  Numeric. Precipitation Seasonality

- BIO16:

  Numeric. Precipitation of Wettest Quarter

- BIO17:

  Numeric. Precipitation of Driest Quarter

- BIO18:

  Numeric. Precipitation of Warmest Quarter

- BIO19:

  Numeric. Precipitation of Coldest Quarter

## Source

Simulated data based on the 19 WorldClim bioclimatic variables

## Details

This dataset demonstrates a common problem in ecological modeling:
bioclimatic predictors are highly correlated within groups (temperature
variables BIO1-BIO11 are highly correlated; precipitation variables
BIO12-BIO19 are moderately correlated), leading to multicollinearity
issues. The species richness response depends on a subset of predictors.

**Use case**: Demonstrating
[`corrPrune()`](https://gillescolling.com/corrselect/reference/corrPrune.md)
and
[`modelPrune()`](https://gillescolling.com/corrselect/reference/modelPrune.md)
for reducing correlated environmental predictors before fitting species
distribution models.

## See also

[`corrPrune()`](https://gillescolling.com/corrselect/reference/corrPrune.md),
[`modelPrune()`](https://gillescolling.com/corrselect/reference/modelPrune.md)

## Examples

``` r
data(bioclim_example)

# The 19 WorldClim bioclimatic variables (https://www.worldclim.org/data/bioclim.html)
# Many are highly correlated, making them ideal for pruning

# Remove highly correlated variables
pruned <- corrPrune(bioclim_example[, -1], threshold = 0.7)
ncol(pruned)  # Reduced from 19 to ~8 variables
#> [1] 12

# Model-based pruning with VIF
model_data <- modelPrune(species_richness ~ .,
                         data = bioclim_example,
                         limit = 5)
attr(model_data, "selected_vars")
#>  [1] "BIO1"  "BIO3"  "BIO4"  "BIO6"  "BIO8"  "BIO9"  "BIO10" "BIO11" "BIO12"
#> [10] "BIO13" "BIO14" "BIO15" "BIO16" "BIO17" "BIO18" "BIO19"
```

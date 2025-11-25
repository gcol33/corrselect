# Example Gene Expression Data for Bioinformatics

A simulated gene expression dataset with 200 genes measured across 100
samples, organized into co-expression modules with a binary disease
outcome.

## Usage

``` r
genes_example
```

## Format

A data frame with 100 rows and 202 variables:

- sample_id:

  Character. Unique sample identifier

- disease_status:

  Factor. Disease status (Healthy, Disease)

- GENE001, GENE002, GENE003, GENE004, GENE005, GENE006, GENE007,
  GENE008, GENE009, GENE010, GENE011, GENE012, GENE013, GENE014,
  GENE015, GENE016, GENE017, GENE018, GENE019, GENE020, GENE021,
  GENE022, GENE023, GENE024, GENE025, GENE026, GENE027, GENE028,
  GENE029, GENE030, GENE031, GENE032, GENE033, GENE034, GENE035,
  GENE036, GENE037, GENE038, GENE039, GENE040, GENE041, GENE042,
  GENE043, GENE044, GENE045, GENE046, GENE047, GENE048, GENE049,
  GENE050, GENE051, GENE052, GENE053, GENE054, GENE055, GENE056,
  GENE057, GENE058, GENE059, GENE060, GENE061, GENE062, GENE063,
  GENE064, GENE065, GENE066, GENE067, GENE068, GENE069, GENE070,
  GENE071, GENE072, GENE073, GENE074, GENE075, GENE076, GENE077,
  GENE078, GENE079, GENE080, GENE081, GENE082, GENE083, GENE084,
  GENE085, GENE086, GENE087, GENE088, GENE089, GENE090, GENE091,
  GENE092, GENE093, GENE094, GENE095, GENE096, GENE097, GENE098,
  GENE099, GENE100, GENE101, GENE102, GENE103, GENE104, GENE105,
  GENE106, GENE107, GENE108, GENE109, GENE110, GENE111, GENE112,
  GENE113, GENE114, GENE115, GENE116, GENE117, GENE118, GENE119,
  GENE120, GENE121, GENE122, GENE123, GENE124, GENE125, GENE126,
  GENE127, GENE128, GENE129, GENE130, GENE131, GENE132, GENE133,
  GENE134, GENE135, GENE136, GENE137, GENE138, GENE139, GENE140,
  GENE141, GENE142, GENE143, GENE144, GENE145, GENE146, GENE147,
  GENE148, GENE149, GENE150, GENE151, GENE152, GENE153, GENE154,
  GENE155, GENE156, GENE157, GENE158, GENE159, GENE160, GENE161,
  GENE162, GENE163, GENE164, GENE165, GENE166, GENE167, GENE168,
  GENE169, GENE170, GENE171, GENE172, GENE173, GENE174, GENE175,
  GENE176, GENE177, GENE178, GENE179, GENE180, GENE181, GENE182,
  GENE183, GENE184, GENE185, GENE186, GENE187, GENE188, GENE189,
  GENE190, GENE191, GENE192, GENE193, GENE194, GENE195, GENE196,
  GENE197, GENE198, GENE199, GENE200:

  Numeric. Gene expression values (log-transformed)

## Source

Simulated data based on typical gene expression microarray structures

## Details

This dataset simulates a high-dimensional, low-sample scenario common in
genomics. Genes are organized into four co-expression modules:

- Module 1 (GENE001-GENE050): Highly correlated (r ~= 0.80),
  disease-associated

- Module 2 (GENE051-GENE100): Moderately correlated (r ~= 0.60)

- Module 3 (GENE101-GENE150): Weakly correlated (r ~= 0.40)

- Module 4 (GENE151-GENE200): Independent (r ~= 0)

Disease outcome depends on a subset of genes from Module 1.

**Use case**: Demonstrating
[`corrPrune()`](https://gillescolling.com/corrselect/reference/corrPrune.md)
with `mode = "greedy"` for handling high-dimensional data efficiently.

## See also

[`corrPrune()`](https://gillescolling.com/corrselect/reference/corrPrune.md)

## Examples

``` r
data(genes_example)

# Greedy pruning for high-dimensional data
gene_data <- genes_example[, -(1:2)]  # Exclude ID and outcome
pruned <- corrPrune(gene_data, threshold = 0.8, mode = "greedy")
ncol(pruned)  # Reduced from 200 to ~50 genes
#> [1] 177

# Use pruned genes for classification
pruned_with_outcome <- data.frame(
  disease_status = genes_example$disease_status,
  pruned
)
```

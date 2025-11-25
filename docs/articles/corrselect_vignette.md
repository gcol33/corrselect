# Correlation Subset Selection with corrselect

## Overview

`corrselect` identifies all maximal subsets of variables whose pairwise
correlations stay below a chosen threshold. This process reduces
multicollinearity and redundancy before modeling, while preserving
interpretability. Unlike greedy or stepwise approaches, `corrselect`
exhaustively searches for all valid subsets using fast, exact
algorithms. It is fully model-agnostic, making it suitable as a
preprocessing step for regression, clustering, feature selection, and
other analyses.

Given a threshold $`t \in (0,1)`$, the functions
[`corrSelect()`](https://gcol33.github.io/corrselect/reference/corrSelect.md)
(data-frame interface) and
[`MatSelect()`](https://gcol33.github.io/corrselect/reference/MatSelect.md)
(matrix interface) enumerate all **maximal** subsets $`S`$ of variables
satisfying:

``` math
\forall i, j \in S,\ i \neq j: \ |r_{ij}| < t
```

where $`r_{ij}`$ denotes the chosen correlation measure between
variables $`i`$ and $`j`$. Enumeration relies on two exact
graph-theoretic algorithms:

1.  **Eppstein–Löffler–Strash (ELS)**, a degeneracy-ordered backtracking
    algorithm optimized for sparse graphs.  
2.  **Bron–Kerbosch (BK)**, a classical recursive clique-finding method,
    with optional pivoting to reduce search space.

Results are returned as a `CorrCombo` S4 object containing each subset’s
variable names and summary statistics (`avg_corr`, `min_corr`,
`max_corr`). You can then extract subsets from the original data via
[`corrSubset()`](https://gcol33.github.io/corrselect/reference/corrSubset.md).
Because the procedure does not depend on any downstream model, it
cleanly separates “feature curation” from “model fitting” and supports
multiple correlation measures (`pearson`, `spearman`, `kendall`,
`bicor`, `distance`, `maximal`).

------------------------------------------------------------------------

## Quick Start (`CorrSelect`)

### Simulated numeric example

``` r

set.seed(42)
n <- 100
df <- data.frame(
  A = rnorm(n),
  B = rnorm(n),
  C = rnorm(n),
  D = rnorm(n),
  E = rnorm(n)
)
df$F <- df$A * 0.9 + rnorm(n, sd = 0.1)  # strongly correlated with A
```

### Basic selection

``` r

res <- corrSelect(df, threshold = 0.7)
res
#> CorrCombo object
#> -----------------
#>   Method:      bron-kerbosch
#>   Correlation: pearson
#>   Threshold:   0.700
#>   Subsets:     2 valid combinations
#>   Data Rows:   100 used in correlation
#>   Pivot:       TRUE
#> 
#> Top combinations:
#>   No.  Variables                          Avg    Max    Size
#>   ------------------------------------------------------------
#>   [ 1] F, B, C, D, E                     0.082  0.185     5
#>   [ 2] A, B, C, D, E                     0.083  0.185     5
as.data.frame(res)
#>                      VarName01 VarName02 VarName03 VarName04 VarName05
#> Subset01 [avg=0.082]         F         B         C         D         E
#> Subset02 [avg=0.083]         A         B         C         D         E
```

``` r

corrSubset(res, df, which = 1)[1:10,]
#>              F          B          C            D           E
#> 1   1.33677667  1.2009654 -2.0009292 -0.004620768  1.33491259
#> 2  -0.41675087  1.0447511  0.3337772  0.760242168 -0.86927176
#> 3   0.32656994 -1.0032086  1.1713251  0.038990913  0.05548695
#> 4   0.58317730  1.8484819  2.0595392  0.735072142  0.04906691
#> 5   0.29182614 -0.6667734 -1.3768616 -0.146472627 -0.57835573
#> 6  -0.11532450  0.1055138 -1.1508556 -0.057887335 -0.99873866
#> 7   1.25744892 -0.4222559 -0.7058214  0.482369466 -0.00243278
#> 8  -0.18188872 -0.1223502 -1.0540558  0.992943637  0.65551188
#> 9   1.69450003  0.1881930 -0.6457437 -1.246395498  1.47684228
#> 10  0.02717808  0.1191610 -0.1853780 -0.033487525 -1.90915279
```

### Forcing variables into all subsets

``` r

res2 <- corrSelect(df, threshold = 0.7, force_in = "A")
res2
#> CorrCombo object
#> -----------------
#>   Method:      els
#>   Correlation: pearson
#>   Threshold:   0.700
#>   Subsets:     1 valid combinations
#>   Data Rows:   100 used in correlation
#>   Forced-in:   A
#> 
#> Top combinations:
#>   No.  Variables                          Avg    Max    Size
#>   ------------------------------------------------------------
#>   [ 1] A, B, C, D, E                     0.083  0.185     5
```

### Using a different correlation method

``` r

res3 <- corrSelect(df, threshold = 0.6, cor_method = "spearman")
res3
#> CorrCombo object
#> -----------------
#>   Method:      bron-kerbosch
#>   Correlation: spearman
#>   Threshold:   0.600
#>   Subsets:     2 valid combinations
#>   Data Rows:   100 used in correlation
#>   Pivot:       TRUE
#> 
#> Top combinations:
#>   No.  Variables                          Avg    Max    Size
#>   ------------------------------------------------------------
#>   [ 1] F, B, C, D, E                     0.088  0.191     5
#>   [ 2] A, B, C, D, E                     0.090  0.206     5
```

## Matrix Interface (`MatSelect`)

If you already computed a correlation matrix or want to apply the method
to precomputed correlations:

``` r

mat <- cor(df)
res4 <- MatSelect(mat, threshold = 0.7)
res4
#> CorrCombo object
#> -----------------
#>   Method:      bron-kerbosch
#>   Threshold:   0.700
#>   Subsets:     2 valid combinations
#>   Data Rows:   6 used in correlation
#>   Pivot:       TRUE
#> 
#> Top combinations:
#>   No.  Variables                          Avg    Max    Size
#>   ------------------------------------------------------------
#>   [ 1] F, B, C, D, E                     0.082  0.185     5
#>   [ 2] A, B, C, D, E                     0.083  0.185     5
```

Selecting subsets:

``` r

MatSelect(mat, threshold = 0.5)
#> CorrCombo object
#> -----------------
#>   Method:      bron-kerbosch
#>   Threshold:   0.500
#>   Subsets:     2 valid combinations
#>   Data Rows:   6 used in correlation
#>   Pivot:       TRUE
#> 
#> Top combinations:
#>   No.  Variables                          Avg    Max    Size
#>   ------------------------------------------------------------
#>   [ 1] F, B, C, D, E                     0.082  0.185     5
#>   [ 2] A, B, C, D, E                     0.083  0.185     5
```

Force variable 1 into every subset:

``` r

MatSelect(mat, threshold = 0.5, force_in = 1)
#> CorrCombo object
#> -----------------
#>   Method:      els
#>   Threshold:   0.500
#>   Subsets:     1 valid combinations
#>   Data Rows:   6 used in correlation
#>   Forced-in:   A
#> 
#> Top combinations:
#>   No.  Variables                          Avg    Max    Size
#>   ------------------------------------------------------------
#>   [ 1] A, B, C, D, E                     0.083  0.185     5
```

## Mixed Data Types (`assocSelect`)

``` r

df_ass <- data.frame(
  height = rnorm(15, 170, 10),
  weight = rnorm(15, 70, 12),
  group  = factor(rep(LETTERS[1:3], each = 5)),
  score  = ordered(sample(c("low","med","high"), 15, TRUE))
)

# keep every subset whose internal associations ≤ 0.6
res5 <- assocSelect(df_ass, threshold = 0.6)
res5
#> CorrCombo object
#> -----------------
#>   Method:      bron-kerbosch
#>   Correlation: mixed
#>   AssocMethod: numeric_numeric = pearson, numeric_factor = eta, numeric_ordered
#>                = spearman, factor_ordered = cramersv
#>   Threshold:   0.600
#>   Subsets:     1 valid combinations
#>   Data Rows:   15 used in correlation
#>   Pivot:       TRUE
#> 
#> Top combinations:
#>   No.  Variables                          Avg    Max    Size
#>   ------------------------------------------------------------
#>   [ 1] height, weight, group, score      0.267  0.554     4
```

------------------------------------------------------------------------

## Changing Correlation Method

By default,
[`corrSelect()`](https://gcol33.github.io/corrselect/reference/corrSelect.md)
uses Pearson correlation. You can choose alternatives with the
`cor_method` argument:

- `"pearson"`: linear correlation (default)  
- `"spearman"`: rank-based monotonic association  
- `"kendall"`: Kendall’s tau  
- `"bicor"`: robust biweight midcorrelation
  ([`WGCNA::bicor`](https://rdrr.io/pkg/WGCNA/man/bicor.html))  
- `"distance"`: distance correlation
  ([`energy::dcor`](https://rdrr.io/pkg/energy/man/dcov.html))  
- `"maximal"`: maximal information coefficient
  ([`minerva::mine`](https://rdrr.io/pkg/minerva/man/mine.html))

Example:

``` r

res6 <- corrSelect(df, threshold = 0.7, cor_method = "spearman")
res6
#> CorrCombo object
#> -----------------
#>   Method:      bron-kerbosch
#>   Correlation: spearman
#>   Threshold:   0.700
#>   Subsets:     2 valid combinations
#>   Data Rows:   100 used in correlation
#>   Pivot:       TRUE
#> 
#> Top combinations:
#>   No.  Variables                          Avg    Max    Size
#>   ------------------------------------------------------------
#>   [ 1] F, B, C, D, E                     0.088  0.191     5
#>   [ 2] A, B, C, D, E                     0.090  0.206     5
```

------------------------------------------------------------------------

## Handling Mixed Data Types

The function
[`assocSelect()`](https://gcol33.github.io/corrselect/reference/assocSelect.md)
extends
[`corrSelect()`](https://gcol33.github.io/corrselect/reference/corrSelect.md)
to support **mixed data types** — including numeric, factor, and ordered
variables — by using appropriate association measures for each variable
pair.

Instead of a single correlation matrix, it constructs a **generalized
association matrix** using the following logic:

| Variable 1 | Variable 2 | Method Used                        |
|------------|------------|------------------------------------|
| numeric    | numeric    | `pearson` (default; customizable)  |
| numeric    | factor     | `eta`                              |
| numeric    | ordered    | `spearman` (default; customizable) |
| factor     | factor     | `cramersv`                         |
| factor     | ordered    | `cramersv`                         |
| ordered    | ordered    | `spearman` (default; customizable) |

The defaults for numeric-numeric, numeric-ordered, and ordered-ordered
associations can be changed via arguments:

``` r

assocSelect(df_ass,
  method_num_num = "kendall",
  method_num_ord = "spearman",
  method_ord_ord = "kendall"
)
#> CorrCombo object
#> -----------------
#>   Method:      bron-kerbosch
#>   Correlation: mixed
#>   AssocMethod: numeric_numeric = kendall, numeric_factor = eta, numeric_ordered
#>                = spearman, factor_ordered = cramersv
#>   Threshold:   0.700
#>   Subsets:     1 valid combinations
#>   Data Rows:   15 used in correlation
#>   Pivot:       TRUE
#> 
#> Top combinations:
#>   No.  Variables                          Avg    Max    Size
#>   ------------------------------------------------------------
#>   [ 1] height, weight, group, score      0.270  0.554     4
```

All other combinations use fixed methods (`eta` or `cramersv`)
appropriate for measuring association strength.

### Example with Mixed Types

``` r

df_ass <- data.frame(
  height = rnorm(10),
  weight = rnorm(10),
  group  = factor(sample(c("A", "B"), 10, replace = TRUE)),
  score  = ordered(sample(1:3, 10, replace = TRUE))
)

res7 <- assocSelect(df_ass, threshold = 1, method = "bron-kerbosch", use_pivot = TRUE)
res7
#> CorrCombo object
#> -----------------
#>   Method:      bron-kerbosch
#>   Correlation: mixed
#>   AssocMethod: numeric_numeric = pearson, numeric_factor = eta, numeric_ordered
#>                = spearman, factor_ordered = cramersv
#>   Threshold:   1.000
#>   Subsets:     1 valid combinations
#>   Data Rows:   10 used in correlation
#>   Pivot:       TRUE
#> 
#> Top combinations:
#>   No.  Variables                          Avg    Max    Size
#>   ------------------------------------------------------------
#>   [ 1] height, weight, group, score      0.336  0.495     4
```

Each pairwise association is bounded to \[0,1\] and treated analogously
to correlation.

------------------------------------------------------------------------

## Theory

Given a symmetric correlation matrix $`R \in \mathbb{R}^{p \times p}`$,
we seek all **maximal subsets** $`S \subseteq \{1, \dots, p\}`$ such
that:

``` math
\forall i, j \in S,\ i \neq j: \ |R_{ij}| < t
```

for a fixed threshold $`t \in (0, 1)`$.

This is equivalent to finding all **maximal cliques** in the
**thresholded correlation graph**, where:

- Nodes represent variables  
- Edges connect nodes whose absolute correlation is **below** the
  threshold

A maximal clique corresponds to a variable subset that cannot be
extended without violating the correlation limit.

------------------------------------------------------------------------

## Algorithms

### ELS (Eppstein–Löffler–Strash)

The ELS algorithm efficiently enumerates all maximal cliques in a sparse
graph using **degeneracy ordering**:

1.  Compute a degeneracy ordering $`v_1, \dots, v_p`$.  
2.  For each $`i`$, extend current clique $`S`$ from $`\{v_i\}`$ within
    candidate set $`C = \{v_{i+1}, \dots, v_p\}`$.  
3.  Recursively build cliques, pruning when no further vertices can be
    added.

Formally, define:

``` math
\text{extend}(S, C) =
\begin{cases}
S, & C = \emptyset, \\
\bigcup_{v \in C} \text{extend}(S \cup \{v\},\ C \setminus (N(v) \cup \{v\})), & \text{otherwise}.
\end{cases}
```

ELS avoids redundant exploration, achieving good performance on typical
correlation graphs.

### Bron–Kerbosch (with Pivoting)

The classical Bron–Kerbosch algorithm enumerates maximal cliques via
recursive backtracking with optional pivoting:

Let $`R`$ = current clique, $`P`$ = prospective nodes, $`X`$ = excluded
nodes. Then:

``` math
\text{BK}(R, P, X) =
\begin{cases}
\text{report}(R), & P = X = \emptyset, \\
\text{for each } v \in P \setminus N(u): \\
\quad \text{BK}(R \cup \{v\},\ P \cap N(v),\ X \cap N(v)), \
\quad P \leftarrow P \setminus \{v\},\ X \leftarrow X \cup \{v\}.
\end{cases}
```

Choosing a pivot $`u \in P \cup X`$ and iterating over
$`P \setminus N(u)`$ reduces recursive calls.

------------------------------------------------------------------------

## Why corrselect?

Most existing R tools:

- Filter one variable at a time (e.g. `findCorrelation`)  
- Use greedy or backward-selection heuristics  
- Do not enumerate **all** valid subsets

**corrselect** uniquely provides:

- **Exact** enumeration of maximal subsets  
- Support for multiple correlation measures  
- Optional forcing of variables  
- Full inspection via `CorrCombo` objects  
- Fast C++ implementations via Rcpp

This makes it ideal for pipelines where **interpretability** and
**completeness** are essential.

------------------------------------------------------------------------

## Inspecting Results

Convert results for downstream use:

``` r

df_res <- as.data.frame(res)
head(df_res)
#>                      VarName01 VarName02 VarName03 VarName04 VarName05
#> Subset01 [avg=0.082]         F         B         C         D         E
#> Subset02 [avg=0.083]         A         B         C         D         E
```

Extract individual subsets:

``` r

lapply(corrSubset(res, df, which = 1:2), function(x) head(x, 10))
#> $Subset1
#>              F          B          C            D           E
#> 1   1.33677667  1.2009654 -2.0009292 -0.004620768  1.33491259
#> 2  -0.41675087  1.0447511  0.3337772  0.760242168 -0.86927176
#> 3   0.32656994 -1.0032086  1.1713251  0.038990913  0.05548695
#> 4   0.58317730  1.8484819  2.0595392  0.735072142  0.04906691
#> 5   0.29182614 -0.6667734 -1.3768616 -0.146472627 -0.57835573
#> 6  -0.11532450  0.1055138 -1.1508556 -0.057887335 -0.99873866
#> 7   1.25744892 -0.4222559 -0.7058214  0.482369466 -0.00243278
#> 8  -0.18188872 -0.1223502 -1.0540558  0.992943637  0.65551188
#> 9   1.69450003  0.1881930 -0.6457437 -1.246395498  1.47684228
#> 10  0.02717808  0.1191610 -0.1853780 -0.033487525 -1.90915279
#> 
#> $Subset2
#>              A          B          C            D           E
#> 1   1.37095845  1.2009654 -2.0009292 -0.004620768  1.33491259
#> 2  -0.56469817  1.0447511  0.3337772  0.760242168 -0.86927176
#> 3   0.36312841 -1.0032086  1.1713251  0.038990913  0.05548695
#> 4   0.63286260  1.8484819  2.0595392  0.735072142  0.04906691
#> 5   0.40426832 -0.6667734 -1.3768616 -0.146472627 -0.57835573
#> 6  -0.10612452  0.1055138 -1.1508556 -0.057887335 -0.99873866
#> 7   1.51152200 -0.4222559 -0.7058214  0.482369466 -0.00243278
#> 8  -0.09465904 -0.1223502 -1.0540558  0.992943637  0.65551188
#> 9   2.01842371  0.1881930 -0.6457437 -1.246395498  1.47684228
#> 10 -0.06271410  0.1191610 -0.1853780 -0.033487525 -1.90915279
```

Summarize correlation metrics:

``` r

# Number and size of subsets
length(res@subset_list)
#> [1] 2
summary(lengths(res@subset_list))
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>       5       5       5       5       5       5

# Summaries of within-subset correlations
summary(res@max_corr)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>   0.185   0.185   0.185   0.185   0.185   0.185
summary(res@avg_corr)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#> 0.08162 0.08185 0.08208 0.08208 0.08232 0.08255
```

------------------------------------------------------------------------

## CorrCombo Object Structure

A `CorrCombo` S4 object contains:

- `subset_list`: list of character vectors (variable names)  
- `avg_corr`, `min_corr`, `max_corr`: numeric vectors of correlation
  metrics  
- `threshold`, `forced_in`, `search_type`, `cor_method`, `n_rows_used`  
- Attribute `use_pivot` (if applicable)

Inspect slots:

``` r

str(res@subset_list)
#> List of 2
#>  $ : chr [1:5] "F" "B" "C" "D" ...
#>  $ : chr [1:5] "A" "B" "C" "D" ...
```

------------------------------------------------------------------------

## Session Info

``` r

sessionInfo()
#> R version 4.5.1 (2025-06-13 ucrt)
#> Platform: x86_64-w64-mingw32/x64
#> Running under: Windows 11 x64 (build 26200)
#> 
#> Matrix products: default
#>   LAPACK version 3.12.1
#> 
#> locale:
#> [1] LC_COLLATE=English_United States.utf8 
#> [2] LC_CTYPE=English_United States.utf8   
#> [3] LC_MONETARY=English_United States.utf8
#> [4] LC_NUMERIC=C                          
#> [5] LC_TIME=English_United States.utf8    
#> 
#> time zone: Europe/Luxembourg
#> tzcode source: internal
#> 
#> attached base packages:
#> [1] stats     graphics  grDevices utils     datasets  methods   base     
#> 
#> other attached packages:
#> [1] corrselect_3.0.0
#> 
#> loaded via a namespace (and not attached):
#>  [1] digest_0.6.37     desc_1.4.3        R6_2.6.1          fastmap_1.2.0    
#>  [5] xfun_0.53         cachem_1.1.0      knitr_1.50        htmltools_0.5.8.1
#>  [9] rmarkdown_2.30    lifecycle_1.0.4   cli_3.6.5         sass_0.4.10      
#> [13] pkgdown_2.1.3     textshaping_1.0.3 jquerylib_0.1.4   systemfonts_1.2.3
#> [17] compiler_4.5.1    tools_4.5.1       ragg_1.5.0        evaluate_1.0.5   
#> [21] bslib_0.9.0       Rcpp_1.1.0        yaml_2.3.10       jsonlite_2.0.0   
#> [25] rlang_1.1.6       fs_1.6.6          htmlwidgets_1.6.4
```

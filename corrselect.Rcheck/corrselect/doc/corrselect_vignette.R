## ----include=FALSE------------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(corrselect)

## -----------------------------------------------------------------------------
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

## -----------------------------------------------------------------------------
res <- corrSelect(df, threshold = 0.7)
res
as.data.frame(res)

## -----------------------------------------------------------------------------
corrSubset(res, df, which = 1)[1:10,]

## -----------------------------------------------------------------------------
res2 <- corrSelect(df, threshold = 0.7, force_in = "A")
res2

## -----------------------------------------------------------------------------
res3 <- corrSelect(df, threshold = 0.6, cor_method = "spearman")
res3

## -----------------------------------------------------------------------------
mat <- cor(df)
res4 <- MatSelect(mat, threshold = 0.7)
res4

## -----------------------------------------------------------------------------
MatSelect(mat, threshold = 0.5)

## -----------------------------------------------------------------------------
MatSelect(mat, threshold = 0.5, force_in = 1)

## -----------------------------------------------------------------------------
df_ass <- data.frame(
  height = rnorm(15, 170, 10),
  weight = rnorm(15, 70, 12),
  group  = factor(rep(LETTERS[1:3], each = 5)),
  score  = ordered(sample(c("low","med","high"), 15, TRUE))
)

# keep every subset whose internal associations ≤ 0.6
res5 <- assocSelect(df_ass, threshold = 0.6)
res5

## -----------------------------------------------------------------------------
res6 <- corrSelect(df, threshold = 0.7, cor_method = "spearman")
res6

## -----------------------------------------------------------------------------
assocSelect(df_ass,
  method_num_num = "kendall",
  method_num_ord = "spearman",
  method_ord_ord = "kendall"
)

## -----------------------------------------------------------------------------
df_ass <- data.frame(
  height = rnorm(10),
  weight = rnorm(10),
  group  = factor(sample(c("A", "B"), 10, replace = TRUE)),
  score  = ordered(sample(1:3, 10, replace = TRUE))
)

res7 <- assocSelect(df_ass, threshold = 1, method = "bron-kerbosch", use_pivot = TRUE)
res7

## -----------------------------------------------------------------------------
df_res <- as.data.frame(res)
head(df_res)

## -----------------------------------------------------------------------------
lapply(corrSubset(res, df, which = 1:2), function(x) head(x, 10))

## -----------------------------------------------------------------------------
# Number and size of subsets
length(res@subset_list)
summary(lengths(res@subset_list))

# Summaries of within-subset correlations
summary(res@max_corr)
summary(res@avg_corr)

## -----------------------------------------------------------------------------
str(res@subset_list)

## -----------------------------------------------------------------------------
sessionInfo()


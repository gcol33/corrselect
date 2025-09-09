## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----spectral-prototype, eval=FALSE-------------------------------------------
# set.seed(1)
# mat <- matrix(rnorm(100), ncol = 10)
# colnames(mat) <- paste0("V", 1:10)
# cmat <- cor(mat)
# 
# res <- MatSelect(cmat, threshold = 0.5, method = "spectral")
# res

## ----spectral-k-param, eval=FALSE---------------------------------------------
# res <- MatSelect(cmat, threshold = 0.5, method = "spectral", k = 4)


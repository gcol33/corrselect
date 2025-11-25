## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5
)

## -----------------------------------------------------------------------------
library(corrselect)

# Construct a simple 4x4 correlation matrix
cor_4var <- matrix(c(
  1.00, 0.85, 0.10, 0.15,
  0.85, 1.00, 0.12, 0.18,
  0.10, 0.12, 1.00, 0.75,
  0.15, 0.18, 0.75, 1.00
), nrow = 4, byrow = TRUE)

colnames(cor_4var) <- rownames(cor_4var) <- paste0("V", 1:4)

# Display matrix
print(cor_4var)

## ----fig.width=6, fig.height=4, fig.alt="Text output showing adjacency matrix for threshold graph with 4 variables. Matrix displays 1 where edges exist (variables with absolute correlation below 0.7 can coexist) and 0 where edges don't exist (variables with absolute correlation above 0.7 cannot coexist). The pattern reveals which variable pairs are compatible for inclusion in the same maximal subset."----
# Adjacency matrix for threshold graph (edges where |cor| < 0.7)
adj_matrix <- abs(cor_4var) < 0.7
diag(adj_matrix) <- FALSE  # No self-loops

# Visualize as adjacency matrix
cat("Threshold graph edges (1 = edge exists):\n")
print(adj_matrix * 1)

## ----fig.width=8, fig.height=6, fig.alt="Graph visualization with 4 nodes (V1, V2, V3, V4) arranged in a square pattern. Blue edges connect variable pairs with absolute correlation below 0.7 threshold. Red labels on edges show actual correlation values. Large red circles highlight nodes, with white labels. The graph structure reveals two maximal cliques: {V1,V3} and {V2,V4}, corresponding to maximal subsets where all pairwise correlations remain below threshold."----
# Node positions (arranged in a square for clarity)
node_pos <- matrix(c(
  0, 1,    # V1 (top-left)
  2, 1,    # V2 (top-right)
  0, 0,    # V3 (bottom-left)
  2, 0     # V4 (bottom-right)
), ncol = 2, byrow = TRUE)

# Plot setup
par(mar = c(2, 2, 3, 2))
plot(node_pos, type = "n", xlim = c(-0.5, 2.5), ylim = c(-0.5, 1.5),
     xlab = "", ylab = "", axes = FALSE,
     main = "Threshold Graph (τ = 0.7)\nEdges connect variables with |correlation| < 0.7")

# Draw edges (where correlation < 0.7)
edge_color <- rgb(0.2, 0.5, 0.8, 0.6)
edge_lwd <- 2

for (i in 1:4) {
  for (j in 1:4) {
    if (i < j && adj_matrix[i, j]) {
      segments(node_pos[i, 1], node_pos[i, 2],
               node_pos[j, 1], node_pos[j, 2],
               col = edge_color, lwd = edge_lwd)
    }
  }
}

# Draw nodes
node_size <- 0.15
for (i in 1:4) {
  # Node circle
  symbols(node_pos[i, 1], node_pos[i, 2],
          circles = node_size, add = TRUE,
          inches = FALSE, bg = "white", fg = "black", lwd = 2)

  # Node label
  text(node_pos[i, 1], node_pos[i, 2],
       labels = paste0("V", i), cex = 1.2, font = 2)
}

# Add correlation annotations
text(1, 1.35, "cor = 0.85\n(too high, no edge)", cex = 0.8, col = "red")
text(1, -0.35, "cor = 0.75\n(too high, no edge)", cex = 0.8, col = "red")

# Legend showing maximal cliques
legend("right",
       legend = c("Maximal cliques:", "{V1, V3}", "{V1, V4}", "{V2, V3}", "{V2, V4}"),
       bty = "n", cex = 0.9,
       pch = c(NA, 19, 19, 19, 19),
       col = c(NA, "black", "black", "black", "black"))

# Add box around graph
box()

## ----fig.width=8, fig.height=8, fig.alt="Network graph visualization of 20 variables organized into 4 correlation blocks. Nodes are colored by block: red (Block 1, V1-V5, high correlation), orange (Block 2, V6-V10, moderate), light blue (Block 3, V11-V15, low), and dark blue (Block 4, V16-V20, minimal). Gray edges connect variables with absolute correlation below 0.7 threshold. The force-directed layout clusters highly correlated variables together, revealing the block structure. Variables within blocks have few connections (high correlation), while variables across blocks have many connections (low correlation), illustrating which combinations can form maximal cliques."----
data(cor_example)

# Build threshold graph (edges where |correlation| < 0.7)
threshold <- 0.7
adj_mat <- abs(cor_example) < threshold
diag(adj_mat) <- FALSE

if (requireNamespace("igraph", quietly = TRUE)) {
  library(igraph)

  # Create graph from adjacency matrix
  g <- graph_from_adjacency_matrix(adj_mat, mode = "undirected")

  # Find maximal cliques
  cliques <- max_cliques(g)
  cat(sprintf("Found %d maximal cliques at threshold %.1f\n", length(cliques), threshold))

  # Color nodes by which block they belong to
  block_colors <- c(rep("#d73027", 5),   # Block 1 (V1-V5): high correlation
                    rep("#fc8d59", 5),   # Block 2 (V6-V10): moderate
                    rep("#91bfdb", 5),   # Block 3 (V11-V15): low
                    rep("#4575b4", 5))   # Block 4 (V16-V20): minimal

  # Plot network
  par(mar = c(1, 1, 3, 1))
  plot(g,
       vertex.size = 10,
       vertex.color = block_colors,
       vertex.label.cex = 0.8,
       vertex.label.color = "black",
       edge.color = rgb(0.5, 0.5, 0.5, 0.3),
       edge.width = 1,
       layout = layout_with_fr(g),
       main = sprintf("Threshold Graph (τ = %.1f): Variables with |cor| < %.1f are connected",
                     threshold, threshold))

  # Add legend
  legend("topleft",
         legend = c("Block 1 (V1-V5): High cor",
                   "Block 2 (V6-V10): Moderate cor",
                   "Block 3 (V11-V15): Low cor",
                   "Block 4 (V16-V20): Minimal cor"),
         fill = c("#d73027", "#fc8d59", "#91bfdb", "#4575b4"),
         bty = "n", cex = 0.8)
} else {
  cat("Install igraph for network visualization: install.packages('igraph')\n")
  cat("Adjacency matrix (first 5×5 block):\n")
  print(adj_mat[1:5, 1:5] * 1)
}

## -----------------------------------------------------------------------------
results <- MatSelect(cor_4var, threshold = 0.7, method = "bron-kerbosch")
show(results)

## -----------------------------------------------------------------------------
# Create example correlation matrix
set.seed(123)
cor_6var <- matrix(c(
  1.00, 0.85, 0.75, 0.20, 0.15, 0.10,
  0.85, 1.00, 0.80, 0.25, 0.20, 0.15,
  0.75, 0.80, 1.00, 0.30, 0.25, 0.20,
  0.20, 0.25, 0.30, 1.00, 0.65, 0.55,
  0.15, 0.20, 0.25, 0.65, 1.00, 0.60,
  0.10, 0.15, 0.20, 0.55, 0.60, 1.00
), nrow = 6, byrow = TRUE)

rownames(cor_6var) <- colnames(cor_6var) <- paste0("V", 1:6)

# Display correlation matrix
print(round(cor_6var, 2))

## ----fig.width=8, fig.height=4, fig.alt="Two side-by-side visualizations for 6-variable example. Left panel: correlation matrix heatmap with blue (negative), white (zero), and red (positive) colors, numerical values overlaid, and black dashed lines separating two correlation blocks. Right panel: threshold graph showing 6 nodes positioned in two groups based on correlation structure, with blue edges connecting variables where absolute correlation is below 0.7. The dual visualization demonstrates how correlation matrix structure translates to threshold graph topology, revealing maximal cliques within and across correlation blocks."----
library(corrselect)

# Build adjacency matrix for threshold graph
tau <- 0.7
adj_matrix <- abs(cor_6var) < tau
diag(adj_matrix) <- FALSE

# Visualize correlation structure and threshold graph
par(mfrow = c(1, 2))

# Panel 1: Correlation heatmap
col_pal <- colorRampPalette(c("#3B4992", "white", "#EE0000"))(100)
image(1:6, 1:6, t(cor_6var[6:1, ]),
      col = col_pal,
      xlab = "", ylab = "",
      main = "Correlation Matrix",
      axes = FALSE,
      zlim = c(-1, 1))
axis(1, at = 1:6, labels = colnames(cor_6var), cex.axis = 0.8)
axis(2, at = 6:1, labels = colnames(cor_6var), cex.axis = 0.8)

# Add correlation values
for (i in 1:6) {
  for (j in 1:6) {
    col_text <- if (abs(cor_6var[j, i]) > 0.5) "white" else "black"
    text(i, 7 - j, sprintf("%.2f", cor_6var[j, i]),
         cex = 0.7, col = col_text)
  }
}
abline(h = 3.5, lwd = 2, lty = 2, col = "black")
abline(v = 3.5, lwd = 2, lty = 2, col = "black")

# Panel 2: Threshold graph (edges where |cor| < tau)
plot.new()
plot.window(xlim = c(0, 1), ylim = c(0, 1))
title(main = sprintf("Threshold Graph (τ = %.1f)", tau))

# Node positions (2 groups based on correlation structure)
pos <- matrix(c(
  0.2, 0.8,  # V1
  0.3, 0.6,  # V2
  0.1, 0.4,  # V3
  0.7, 0.8,  # V4
  0.8, 0.5,  # V5
  0.9, 0.3   # V6
), ncol = 2, byrow = TRUE)

# Draw edges (compatible variable pairs)
for (i in 1:5) {
  for (j in (i + 1):6) {
    if (adj_matrix[i, j]) {
      lines(c(pos[i, 1], pos[j, 1]), c(pos[i, 2], pos[j, 2]),
            col = "gray70", lwd = 1.5)
    }
  }
}

# Draw nodes
points(pos[, 1], pos[, 2], pch = 21, cex = 4,
       bg = c(rep(rgb(0.8, 0.2, 0.2, 0.7), 3),
              rep(rgb(0.2, 0.5, 0.8, 0.7), 3)),
       col = "black", lwd = 2)

# Add labels
text(pos[, 1], pos[, 2], labels = colnames(cor_6var),
     col = "white", font = 2, cex = 0.8)

# Add legend
legend("bottomleft",
       legend = c("Clique 1 (V1-V3)", "Clique 2 (V4-V6)", "Edge: |cor| < 0.7"),
       pch = c(21, 21, NA),
       pt.bg = c(rgb(0.8, 0.2, 0.2, 0.7), rgb(0.2, 0.5, 0.8, 0.7), NA),
       pt.cex = 2,
       lty = c(NA, NA, 1),
       col = c("black", "black", "gray70"),
       lwd = c(2, 2, 1.5),
       bty = "n",
       cex = 0.7)

par(mfrow = c(1, 1))

## -----------------------------------------------------------------------------
# Run MatSelect to find all maximal subsets
results <- MatSelect(cor_6var, threshold = 0.7, method = "els")
show(results)


# Theory and Formulation

## Overview

This vignette presents the mathematical formulation and graph-theoretic
foundations underlying **corrselect**. Variable subset selection under
correlation constraints is formulated as a maximal independent set
problem on threshold graphs, enabling exact enumeration via established
algorithms from computational graph theory. The vignette defines the
formal problem statement, explains the graph-theoretic representation,
details the three implemented algorithms (Bron-Kerbosch,
Eppstein-Löffler-Strash, and greedy heuristic), analyzes their
computational complexity, and provides comprehensive references to the
theoretical literature. For practical usage examples, see
[`vignette("quickstart")`](https://gillescolling.com/corrselect/articles/quickstart.md)
and
[`vignette("workflows")`](https://gillescolling.com/corrselect/articles/workflows.md).
For algorithmic control and performance tuning, see
[`vignette("advanced")`](https://gillescolling.com/corrselect/articles/advanced.md).

### Contents

1.  [Terminology](#terminology): Core definitions (association,
    threshold, valid subset, clique)

2.  [Intuitive Overview](#intuitive-overview): Conceptual introduction
    with toy examples

3.  [Problem Formulation](#problem-formulation): Formal mathematical
    statement

4.  [Graph-Theoretic Interpretation](#graph-theoretic-interpretation):
    Threshold graphs and maximal cliques

5.  [From Theory to Implementation](#from-theory-to-implementation): How
    concepts map to function arguments

6.  [Search Algorithms](#search-algorithms): Exact enumeration vs greedy
    heuristic

7.  [Algorithm Pseudocode](#algorithm-pseudocode): ELS, Bron-Kerbosch,
    Greedy

8.  [Technical Details](#forced-variables): Forced variables,
    complexity, output structure

9.  [Design Philosophy](#design-philosophy): Why maximal? Why hard
    threshold? Why graphs?

10. [References](#references): Academic literature and further reading

------------------------------------------------------------------------

## Terminology

This section defines the core terms used throughout the documentation.
All other vignettes refer back to these definitions.

### **Association measure**

A symmetric function\
\\a: \mathcal{X} \times \mathcal{X} \to \mathbb{R}\\\
that quantifies the relationship between two variables.

Common cases:

- **Numeric–numeric:** Pearson’s \\\rho\\, Spearman’s \\\rho_s\\,
  Kendall’s \\\tau_K\\

- **Categorical–categorical:** Cramér’s V

- **Numeric–factor:** eta-squared \\\eta^2\\

All measures used in the package are normalized so that\
\\\|a\_{ij}\| \in \[0,1\]\\.

------------------------------------------------------------------------

### **Association matrix**

A symmetric \\p \times p\\ matrix \\A\\ whose entry \\a\_{ij}\\ is the
association between variables \\i\\ and \\j\\.\
The diagonal satisfies \\a\_{ii} = 1\\.\
For correlation-based analysis, \\A\\ typically comes from
[`cor()`](https://rdrr.io/r/stats/cor.html).

------------------------------------------------------------------------

### **Threshold** (\\\tau\\)

A user-defined cutoff in \\(0,1\]\\. Pairs with \\\|a\_{ij}\| \> \tau\\
are considered too strongly associated and cannot both appear in a valid
subset.

Common choices:

- \\\tau = 0.7\\: modelling

- \\\tau = 0.8\\: genomics

- \\\tau = 0.5\\: stringent pruning

------------------------------------------------------------------------

### **Valid subset**

A subset \\S \subseteq \\1,\dots,p\\\\ satisfying\
\\\|a\_{ij}\| \le \tau\\ for all distinct \\i, j \in S\\. All pairwise
associations within \\S\\ remain at or below the threshold.

------------------------------------------------------------------------

### **Maximal valid subset**

A valid subset that cannot be enlarged.\
Formally, no variable \\v \notin S\\ satisfies \\\|a\_{vi}\| \le \tau\\
for all \\i \in S\\.

(“Maximal” is not the same as “maximum”, which refers to the largest
possible subset.)

------------------------------------------------------------------------

### **Threshold graph**

An undirected graph \\G = (V, E)\\ where:

- each vertex in \\V\\ represents a variable

- an edge \\(i,j)\\ exists exactly when \\\|a\_{ij}\| \le \tau\\

Edges therefore connect *compatible* (low-association) variables.

------------------------------------------------------------------------

### **Clique**

A subset of vertices in which every pair is connected by an edge.\
In the threshold graph, cliques correspond to valid subsets.

------------------------------------------------------------------------

### **Maximal clique**

A clique that cannot be extended by adding any additional vertex.\
Maximal cliques correspond exactly to maximal valid subsets.

------------------------------------------------------------------------

### **Forced-in variables** (`force_in`)

A set \\F \subseteq V\\ of variables that must appear in all returned
solutions.\
Only maximal cliques containing all elements of \\F\\ are considered.

------------------------------------------------------------------------

### **ELS (Eppstein–Löffler–Strash)**

A degeneracy-based algorithm for maximal clique enumeration.\
Recommended when `force_in` is used.

Complexity: \\O(d \cdot 3^{d/3})\\, where \\d\\ is the graph’s
degeneracy.

------------------------------------------------------------------------

### **Bron–Kerbosch**

A classical backtracking algorithm for enumerating maximal cliques,
optionally with pivoting.\
Used by default when `force_in` is not specified.

Worst-case complexity: \\O(3^{p/3})\\.

------------------------------------------------------------------------

### **Greedy mode**

A fast heuristic that constructs a single maximal clique via greedy
selection.\
Runs in \\O(p^2)\\.\
Does not guarantee the largest possible subset.

------------------------------------------------------------------------

### **Exact mode**

Enumerates *all* maximal cliques using ELS or Bron–Kerbosch.\
Identifies the maximum (largest) valid subset.

------------------------------------------------------------------------

### **Auto mode**

Chooses the method automatically:

- exact mode for \\p \le\\ `max_exact_p` (default 100)

- greedy mode for \\p \>\\ `max_exact_p`

This balances optimality with computational cost.

> **Key Points: Terminology**
>
> - **Association matrix**: Symmetric matrix of pairwise relationships
>   (correlations, Cramér’s V, etc.)
> - **Threshold (τ)**: Cutoff determining which pairs are “too
>   associated” to coexist
> - **Valid subset**: All pairwise associations below τ
> - **Maximal subset**: Valid subset that cannot be enlarged
> - **Threshold graph**: Graph where edges connect compatible
>   (low-association) variable pairs
> - **Clique**: Fully connected subgraph; maximal cliques = maximal
>   valid subsets

------------------------------------------------------------------------

## Intuitive Overview

Before diving into formal definitions, let’s build intuition with a
simple conceptual overview.

### The Core Idea

Imagine you have a dataset with many predictors, some of which are
highly correlated. For example:

- Temperature at noon and temperature at 2pm (likely correlated ~0.9)

- Monthly income and annual income (perfectly correlated)

- Survey items “I am satisfied” and “I feel happy” (correlated ~0.7)

When building statistical models, including highly correlated predictors
creates problems:

1.  **Coefficient instability**: Small data changes cause large
    coefficient swings

2.  **Inflated variance**: Standard errors become unreliable

3.  **Interpretability issues**: Hard to isolate individual predictor
    effects

The solution: **remove redundant predictors** while keeping as many
variables as possible.

### How corrselect Works

corrselect transforms this statistical problem into a **graph problem**:

1.  **Represent variables as nodes** in a graph

2.  **Draw edges between compatible variables** (correlation at or below
    threshold τ)

3.  **Find maximal groups** where all nodes are connected (maximal
    cliques)

Each maximal clique represents a valid subset: a group of variables
where every pair has correlation at or below τ.

### Why “Maximal” Not “Maximum”?

A **maximal** subset cannot be extended by adding more variables, it’s
locally complete.

A **maximum** subset is the single largest possible subset, globally
optimal.

corrselect finds **all maximal** subsets because:

- Real datasets often have multiple equally good solutions

- You may prefer a smaller subset containing specific variables

- Comparing alternatives reveals correlation structure

- Exact enumeration is feasible for typical problem sizes

### Toy Example (4 Variables)

Consider 4 variables with this correlation matrix:

``` r

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
#>      V1   V2   V3   V4
#> V1 1.00 0.85 0.10 0.15
#> V2 0.85 1.00 0.12 0.18
#> V3 0.10 0.12 1.00 0.75
#> V4 0.15 0.18 0.75 1.00
```

Observations:

- **V1 and V2 are highly correlated** (0.85) - likely redundant

- **V3 and V4 are moderately correlated** (0.75)

- **Between-group correlations are low** (0.10-0.18)

Set threshold τ = 0.7. Which pairs violate the threshold?

- V1-V2: \|0.85\| \> 0.7 ✗ (too high)

- V3-V4: \|0.75\| \> 0.7 ✗ (too high)

- All other pairs: \<= 0.7 ✓ (acceptable)

### Graph Representation

Now we build the **threshold graph** where edges connect *compatible*
variables (correlation at or below 0.7).

**Text representation**:

    Variables: V1, V2, V3, V4

    Edges (|correlation| <= 0.7):
      V1 —— V3  (cor = 0.10)
      V1 —— V4  (cor = 0.15)
      V2 —— V3  (cor = 0.12)
      V2 —— V4  (cor = 0.18)

    Missing edges (|correlation| > 0.7):
      V1 ⨯ V2  (cor = 0.85, too high)
      V3 ⨯ V4  (cor = 0.75, too high)

    Maximal cliques (maximal variable subsets):
      {V1, V3}: Both connected, cannot add V2 or V4
      {V1, V4}: Both connected, cannot add V2 or V3
      {V2, V3}: Both connected, cannot add V1 or V4
      {V2, V4}: Both connected, cannot add V1 or V3

Let’s verify this with code:

``` r

# Adjacency matrix for threshold graph (edges where |cor| <= 0.7)
adj_matrix <- abs(cor_4var) <= 0.7
diag(adj_matrix) <- FALSE  # No self-loops

# Visualize as adjacency matrix
cat("Threshold graph edges (1 = edge exists):\n")
#> Threshold graph edges (1 = edge exists):
print(adj_matrix * 1)
#>    V1 V2 V3 V4
#> V1  0  0  1  1
#> V2  0  0  1  1
#> V3  1  1  0  0
#> V4  1  1  0  0
```

**Interpretation**: An edge exists between Vi and Vj if they can coexist
in a valid subset.

- **V1 connects to**: V3, V4 (not V2)

- **V2 connects to**: V3, V4 (not V1)

- **V3 connects to**: V1, V2 (not V4)

- **V4 connects to**: V1, V2 (not V3)

### Visual Graph Representation

Let’s visualize this threshold graph with nodes and edges:

``` r

# Node positions (arranged in a square for clarity)
node_pos <- matrix(c(
  0, 1,    # V1 (top-left)
  2, 1,    # V2 (top-right)
  0, 0,    # V3 (bottom-left)
  2, 0     # V4 (bottom-right)
), ncol = 2, byrow = TRUE)

# Plot setup, with a right-hand gutter reserved for the clique list
par(mar = c(1, 1, 3, 1))
plot(node_pos, type = "n", xlim = c(-0.4, 4.1), ylim = c(-0.6, 1.6),
     xlab = "", ylab = "", axes = FALSE, asp = 1,
     main = "Threshold Graph (τ = 0.7)")

# Edges are the pairs with correlation < 0.7, and here each one is also a
# maximal clique, so both get the same color
edges <- which(adj_matrix & upper.tri(adj_matrix), arr.ind = TRUE)
edges <- edges[order(edges[, "row"], edges[, "col"]), , drop = FALSE]
edge_cols <- unname(PAL[c("blue", "orange", "teal", "purple")])

for (k in seq_len(nrow(edges))) {
  i <- edges[k, "row"]
  j <- edges[k, "col"]
  segments(node_pos[i, 1], node_pos[i, 2],
           node_pos[j, 1], node_pos[j, 2],
           col = edge_cols[k], lwd = 2)
}

# Draw nodes
node_size <- 0.18
for (i in 1:4) {
  # Node circle
  symbols(node_pos[i, 1], node_pos[i, 2],
          circles = node_size, add = TRUE,
          inches = FALSE, bg = "white", fg = "black", lwd = 2)

  # Node label
  text(node_pos[i, 1], node_pos[i, 2],
       labels = paste0("V", i), cex = 1.25, font = 2)
}

# Add correlation annotations
text(1, 1.45, "cor = 0.85, no edge", col = PAL[["red"]])
text(1, -0.45, "cor = 0.75, no edge", col = PAL[["red"]])

# Maximal cliques, listed in the reserved gutter and colored to their edge
text(2.7, 1.1, "Maximal cliques", font = 2, adj = 0)
clique_labels <- sprintf("{V%d, V%d}", edges[, "row"], edges[, "col"])
text(2.7, 0.75 - (seq_along(clique_labels) - 1) * 0.22, clique_labels,
     col = edge_cols, adj = c(0, 1))
```

![Graph visualization with 4 nodes (V1, V2, V3, V4) arranged in a square
pattern. An edge connects each variable pair with absolute correlation
at or below the 0.7 threshold. Red labels above and below the square
give the two correlations that exceed the threshold, where no edge is
drawn. The graph structure reveals four maximal cliques of size two,
listed to the right of the square: {V1,V3}, {V1,V4}, {V2,V3} and
{V2,V4}, corresponding to maximal subsets where all pairwise
correlations remain at or below threshold. Each clique in the list
carries the color of the edge it comes
from.](theory_files/figure-html/unnamed-chunk-3-1.svg)

**Graph interpretation**:

- **Nodes**: Each variable is a vertex

- **Edges** (colored lines): Connect variables with correlation \< 0.7
  (compatible pairs). Each edge carries the color of the clique it forms
  in the list on the right

- **Missing edges** (no connection): Variables with correlation ≥ 0.7
  (cannot coexist)

  - V1-V2: No edge (corr = 0.85)

  - V3-V4: No edge (corr = 0.75)

**Maximal cliques** (groups where everyone connects to everyone):

1.  {V1, V3}: V1—V3 edge exists ✓, cannot add V2 (no V1—V2 edge) or V4
    (no V3—V4 edge)

2.  {V1, V4}: V1—V4 edge exists ✓, cannot add V2 (no V1—V2 edge) or V3
    (no V3—V4 edge)

3.  {V2, V3}: V2—V3 edge exists ✓, cannot add V1 (no V1—V2 edge) or V4
    (no V3—V4 edge)

4.  {V2, V4}: V2—V4 edge exists ✓, cannot add V1 (no V1—V2 edge) or V3
    (no V3—V4 edge)

This visual representation makes the graph-theoretic formulation
concrete: finding maximal valid variable subsets is equivalent to
finding maximal cliques in the threshold graph.

### Network Visualization with cor_example

For larger matrices, we can use igraph to visualize the threshold graph
structure. Let’s demonstrate with `cor_example`, which has 20 variables
with known block structure:

``` r

data(cor_example)

# Build threshold graph (edges where |correlation| <= 0.7)
threshold <- 0.7
adj_mat <- abs(cor_example) <= threshold
diag(adj_mat) <- FALSE

if (requireNamespace("igraph", quietly = TRUE)) {
  library(igraph)

  # Create graph from adjacency matrix
  g <- graph_from_adjacency_matrix(adj_mat, mode = "undirected")

  # Find maximal cliques
  cliques <- max_cliques(g)
  cat(sprintf("Found %d maximal cliques at threshold %.1f\n", length(cliques), threshold))

  # Color nodes by which block they belong to
  block_pal <- PAL[c("red", "orange", "teal", "blue")]
  block_colors <- rep(block_pal, each = 5)

  # Plot network, with bottom margin reserved for the legend
  par(mar = c(4, 1, 3, 1))
  plot(g,
       vertex.size = 22,
       vertex.color = "white",
       vertex.frame.color = block_colors,
       vertex.frame.width = 2,
       vertex.label.color = block_colors,
       edge.color = adjustcolor(PAL[["grey"]], alpha.f = 0.4),
       edge.width = 1.5,
       layout = layout_with_fr(g),
       main = sprintf("Threshold Graph (τ = %.1f)", threshold))

  # Add legend below the network
  legend("bottom", inset = -0.16, xpd = TRUE, ncol = 2,
         legend = c("Block 1 (V1-V5): high cor",
                   "Block 2 (V6-V10): moderate",
                   "Block 3 (V11-V15): low",
                   "Block 4 (V16-V20): minimal"),
         fill = block_pal, border = block_pal, bty = "n")
} else {
  cat("Install igraph for network visualization: install.packages('igraph')\n")
  cat("Adjacency matrix (first 5×5 block):\n")
  print(adj_mat[1:5, 1:5] * 1)
}
#> 
#> Attaching package: 'igraph'
#> The following objects are masked from 'package:stats':
#> 
#>     decompose, spectrum
#> The following object is masked from 'package:base':
#> 
#>     union
#> Found 5 maximal cliques at threshold 0.7
```

![Network graph visualization of 20 variables organized into 4
correlation blocks. Node outlines and labels are colored by block: red
(Block 1, V1-V5, high correlation), orange (Block 2, V6-V10, moderate),
teal (Block 3, V11-V15, low), and blue (Block 4, V16-V20, minimal). Grey
edges connect variables with absolute correlation at or below 0.7
threshold. The force-directed layout clusters highly correlated
variables together, revealing the block structure. Variables within
blocks have few connections (high correlation), while variables across
blocks have many connections (low correlation), illustrating which
combinations can form maximal
cliques.](theory_files/figure-html/unnamed-chunk-4-1.svg)

**Interpretation**:

- Variables within high-correlation blocks (red, orange) have **few or
  no edges** because their pairwise correlations exceed 0.7

- Variables within low-correlation blocks (light blue, dark blue) have
  **many edges** because their correlations are below 0.7

- Maximal cliques tend to include variables from multiple
  low-correlation blocks

- This explains why corrselect tends to select variables from Block 3
  and Block 4: they have more compatible neighbors

### Finding Maximal Cliques

A **clique** is a group where every pair is connected. In our graph:

**Potential cliques**:

- {V1, V3}: Both connect to each other ✓

- {V1, V4}: Both connect to each other ✓

- {V2, V3}: Both connect to each other ✓

- {V2, V4}: Both connect to each other ✓

- {V1, V3, V4}: Does V3 connect to V4? No ✗

- {V2, V3, V4}: Does V3 connect to V4? No ✗

**Maximal cliques:** Can any 2-variable clique be extended?

- **{V1, V3}**\
  Add V2? No (V1–V2 not connected).\
  Add V4? No (V3–V4 not connected).\
  **Maximal ✓**

- **{V1, V4}**\
  Add V2? No (V1–V2 not connected).\
  Add V3? No (V3–V4 not connected).\
  **Maximal ✓**

- **{V2, V3}**\
  Add V1? No (V1–V2 not connected).\
  Add V4? No (V3–V4 not connected).\
  **Maximal ✓**

- **{V2, V4}**\
  Add V1? No (V1–V2 not connected).\
  Add V3? No (V3–V4 not connected).\
  **Maximal ✓**

So there are **4 maximal cliques of size 2**, each representing a valid
variable subset.

Let corrselect confirm this:

``` r

results <- MatSelect(cor_4var, threshold = 0.7, method = "bron-kerbosch")
show(results)
#> CorrCombo object
#> -----------------
#>   Method:      bron-kerbosch
#>   Threshold:   0.700
#>   Subsets:     4 maximal subsets
#>   Data Rows:   not applicable (matrix input)
#>   Pivot:       TRUE
#> 
#> Top combinations:
#>   No.  Variables                          Avg    Max    Size
#>   ------------------------------------------------------------
#>   [ 1] V1, V3                            0.100  0.100     2
#>   [ 2] V2, V3                            0.120  0.120     2
#>   [ 3] V1, V4                            0.150  0.150     2
#>   [ 4] V2, V4                            0.180  0.180     2
```

**Result interpretation**:

- 4 maximal subsets found (as predicted)

- All have size 2 (cannot be extended further)

- Each satisfies \|correlation\| \<= 0.7 for all pairs

- Mean/max correlations are low (well below threshold)

### Key Insight

This toy example shows why corrselect enumerates all solutions:

- There’s no single “best” subset, all 4 are equally valid

- Choice depends on domain knowledge (which variables are theoretically
  important?)

- Seeing all options reveals the correlation structure (two clusters:
  {V1,V2} and {V3,V4})

Real datasets have similar structure but with more variables and more
complex clustering.

> **Key Points: Intuitive Overview**
>
> - corrselect finds variable subsets where no pair exceeds a
>   correlation threshold
> - Multiple valid subsets typically exist; all are enumerated for user
>   choice
> - Graph representation: variables = nodes, low-correlation pairs =
>   edges
> - Maximal cliques in this graph = maximal valid subsets
> - Comparing subsets reveals correlation structure (clusters of related
>   variables)

------------------------------------------------------------------------

## Problem Formulation

### Intuitive Problem Statement

The core problem is straightforward: given a set of \\p\\ variables with
known pairwise associations (correlations), identify all largest
possible subsets where every pair of variables has an association at or
below a user-defined threshold \\\tau\\.

Think of this as a social network where variables are people and edges
represent “get along well” (low correlation). We want to find all
maximal groups of people where everyone gets along with everyone else in
the group. A group is “maximal” if we cannot add any more people without
introducing a conflict.

Some variables may be designated as “must include” (forced-in). In the
social network analogy, these are VIPs who must be in every group, so we
only search for groups containing all VIPs.

### Formal Problem Statement

**Input:**

- Association matrix \\A \in \mathbb{R}^{p \times p}\\ with \\a\_{ij} =
  a\_{ji}\\ and \\a\_{ii} = 1\\ for all \\i\\

- Threshold \\\tau \in (0, 1\]\\

- Optional forced-in set \\F \subseteq \\1, \dots, p\\\\

**Constraints:**

A subset \\S \subseteq \\1, \dots, p\\\\ is valid if:

1.  \\F \subseteq S\\ (if \\F\\ specified)

2.  \\\|a\_{ij}\| \le \tau\\ for all \\i, j \in S\\ with \\i \neq j\\

**Objective:**

Find all maximal valid subsets \\\mathcal{S} = \\S_1, \dots, S_m\\\\
where \\S_k\\ is maximal if no variable \\v \notin S_k\\ satisfies
\\\|a\_{vi}\| \le \tau\\ for all \\i \in S_k\\.

**Output:**

Collection \\\mathcal{S}\\ containing all maximal valid subsets.

------------------------------------------------------------------------

### Association Matrix

Given \\p\\ variables, compute an association matrix \\A \in
\mathbb{R}^{p \times p}\\ where:

\\ a\_{ij} = \text{association}(X_i, X_j) \\

For numeric variables, \\a\_{ij}\\ may be a correlation coefficient
(Pearson, Spearman, Kendall, etc.). For mixed-type variables,
\\a\_{ij}\\ is chosen based on variable types:

| Type \\(X_i, X_j)\\ | Measure                  |
|---------------------|--------------------------|
| numeric, numeric    | Pearson/Spearman/Kendall |
| numeric, factor     | \\\eta^2\\               |
| numeric, ordered    | Spearman/Kendall         |
| factor, factor      | Cramér’s V               |
| factor, ordered     | Cramér’s V               |
| ordered, ordered    | Spearman/Kendall         |

All measures are bounded: \\a\_{ij} \in \[0, 1\]\\ or \\a\_{ij} \in
\[-1, 1\]\\.

### Threshold Constraint

Fix a threshold \\\tau \in (0, 1\]\\. A subset \\S \subseteq \\1, \dots,
p\\\\ is **valid** if:

\\ \forall i, j \in S,\\ i \neq j: \quad \|a\_{ij}\| \le \tau \\

### Maximal Valid Subsets

A valid subset \\S\\ is **maximal** if no variable \\k \notin S\\
satisfies:

\\ \|a\_{ki}\| \le \tau \quad \text{for all } i \in S \\

> **Key Points: Problem Formulation**
>
> - Input: p × p association matrix A, threshold τ ∈ (0,1\]
> - Valid subset: S where \|a_ij\| \<= τ for all pairs i,j ∈ S
> - Maximal: cannot add any variable without violating threshold
> - Goal: enumerate all maximal valid subsets

------------------------------------------------------------------------

## Graph-Theoretic Interpretation

### Why Graphs?

The variable selection problem has a natural graph representation that
connects it to decades of research in computational graph theory. By
viewing variables as nodes and “compatible pairs” (low correlation) as
edges, we transform the statistical problem into a well-studied graph
problem with proven algorithms.

This representation is powerful because:

1.  **Efficiency**: Graph algorithms exploit structural properties
    (sparsity, degeneracy) for faster computation

2.  **Exactness**: We can enumerate all solutions, not just find one

3.  **Forced variables**: Graph algorithms naturally handle constraints
    (forced-in sets)

The key insight: a group of mutually compatible variables (valid subset)
is exactly a **clique** in a graph where edges represent compatibility.

### Threshold Graph

Define the **threshold graph** \\G = (V, E)\\ where:

- \\V = \\1, \dots, p\\\\ (nodes represent variables)

- \\(i, j) \in E\\ if and only if \\\|a\_{ij}\| \le \tau\\ (edges
  connect compatible variables)

**Note the reversal**: An edge \\(i, j)\\ means variables \\i\\ and
\\j\\ have *low* correlation (can coexist). This is the complement of
the typical “correlation graph” where edges represent high correlation.

### Maximal Cliques

A valid subset \\S\\ corresponds to a **clique** in \\G\\: all pairs in
\\S\\ are connected.

A maximal valid subset corresponds to a **maximal clique**: a clique
that cannot be extended.

Finding all maximal valid subsets is equivalent to enumerating all
maximal cliques in \\G\\.

### Example: 6-Variable Threshold Graph

Consider 6 variables with correlation matrix:

``` r

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
#>      V1   V2   V3   V4   V5   V6
#> V1 1.00 0.85 0.75 0.20 0.15 0.10
#> V2 0.85 1.00 0.80 0.25 0.20 0.15
#> V3 0.75 0.80 1.00 0.30 0.25 0.20
#> V4 0.20 0.25 0.30 1.00 0.65 0.55
#> V5 0.15 0.20 0.25 0.65 1.00 0.60
#> V6 0.10 0.15 0.20 0.55 0.60 1.00
```

Threshold graph construction with \\\tau = 0.7\\:

``` r

# Build adjacency matrix for threshold graph
tau <- 0.7
adj_matrix <- abs(cor_6var) <= tau
diag(adj_matrix) <- FALSE

# Correlation heatmap
col_pal <- colorRampPalette(c("#3B4992", "white", "#EE0000"))(100)
image(1:6, 1:6, t(cor_6var[6:1, ]),
      col = col_pal,
      xlab = "", ylab = "",
      main = "Correlation Matrix",
      axes = FALSE,
      zlim = c(-1, 1))
axis(1, at = 1:6, labels = colnames(cor_6var))
axis(2, at = 6:1, labels = colnames(cor_6var))

# Add correlation values
for (i in 1:6) {
  for (j in 1:6) {
    col_text <- if (abs(cor_6var[j, i]) > 0.5) "white" else "black"
    text(i, 7 - j, sprintf("%.2f", cor_6var[j, i]), col = col_text, font = 2)
  }
}
abline(h = 3.5, lwd = 2, lty = 2, col = "black")
abline(v = 3.5, lwd = 2, lty = 2, col = "black")
```

![Correlation matrix heatmap for the 6-variable example, with blue
(negative), white (zero), and red (positive) colors, numerical values
overlaid on each cell, and black dashed lines separating the two
correlation blocks. Variables V1 to V3 form one high-correlation block
and V4 to V6 another, with low correlations between the
blocks.](theory_files/figure-html/unnamed-chunk-7-1.svg)

The same structure as a threshold graph, where an edge means the pair
can coexist:

``` r

# Threshold graph (edges where |cor| <= tau)
# The bottom margin holds the legend clear of the nodes
par(mar = c(4, 1, 3, 1))
plot.new()
plot.window(xlim = c(0, 1), ylim = c(0, 1))
title(main = sprintf("Threshold Graph (τ = %.1f)", tau))

# Node positions: one block per side, each block a triangle so that a
# within-block edge never runs through the third node
pos <- matrix(c(
  0.20, 0.85,  # V1
  0.05, 0.50,  # V2
  0.20, 0.15,  # V3
  0.80, 0.85,  # V4
  0.95, 0.50,  # V5
  0.80, 0.15   # V6
), ncol = 2, byrow = TRUE)

# Draw edges, separating pairs inside one block from pairs across the two,
# since whether a block is internally compatible is what the cliques turn on
block <- rep(1:2, each = 3)
for (i in 1:5) {
  for (j in (i + 1):6) {
    if (adj_matrix[i, j]) {
      within <- block[i] == block[j]
      lines(c(pos[i, 1], pos[j, 1]), c(pos[i, 2], pos[j, 2]),
            col = if (within) PAL[["orange"]] else PAL[["blue"]],
            lty = if (within) 2 else 1, lwd = 2)
    }
  }
}

# Draw nodes, colored by block
node_cols <- c(rep(PAL[["red"]], 3), rep(PAL[["teal"]], 3))
points(pos[, 1], pos[, 2], pch = 21, cex = 4,
       bg = "white", col = node_cols, lwd = 2)

# Add labels
text(pos[, 1], pos[, 2], labels = colnames(cor_6var),
     col = node_cols, font = 2)

# Add legend
legend("bottom", inset = -0.28, xpd = TRUE,
       legend = c("Block 1 (V1-V3)", "Block 2 (V4-V6)",
                  "Edge across blocks", "Edge within a block"),
       pch = c(21, 21, NA, NA),
       pt.bg = c("white", "white", NA, NA),
       pt.cex = 1.6,
       lty = c(NA, NA, 1, 2),
       col = c(PAL[["red"]], PAL[["teal"]], PAL[["blue"]], PAL[["orange"]]),
       lwd = 2,
       ncol = 2,
       bty = "n")
```

![Threshold graph for the 6-variable example, with the two correlation
blocks on opposite sides: red nodes V1 to V3 on the left, teal nodes V4
to V6 on the right. An edge connects each pair whose absolute
correlation is at or below 0.7. Solid blue edges run between the blocks
and dashed orange edges within one. All nine cross-block pairs have
edges; the only within-block edges are the three among V4, V5 and V6,
and V1 to V3 have none among themselves. Maximal cliques therefore
combine V4, V5 and V6 with one variable from the left
block.](theory_files/figure-html/unnamed-chunk-8-1.svg)

**Interpreting the visualization**:

The correlation matrix has clear block structure. Variables V1-V3 are
highly correlated with each other (correlations 0.75-0.85, shown in
red), as are V4-V6 (correlations 0.55-0.65). Between-block correlations
are low (0.10-0.30, shown in blue/white).

The threshold graph is the same structure at \\\tau = 0.7\\. An edge
connects two variables if their absolute correlation is *at or below*
0.7 (compatible variables). Note that:

- **Within the high-correlation block**: V1-V3 have no edges connecting
  them (every pair exceeds 0.7).

- **Within the moderate block**: V4-V6 are fully connected (0.55-0.65,
  all at or below 0.7).

- **Between blocks**: All V1-V3 to V4-V6 pairs have edges (low
  between-block correlation).

The cliques follow from that structure. \\\\V4, V5, V6\\\\ is a
triangle, and each of V1, V2, V3 connects to all three, so each extends
the triangle to a clique of size 4. V1, V2 and V3 are pairwise
non-adjacent, so at most one of them can appear in any clique. That
leaves three maximal cliques: \\\\V1, V4, V5, V6\\\\, \\\\V2, V4, V5,
V6\\\\ and \\\\V3, V4, V5, V6\\\\.

Identify maximal cliques:

``` r

# Run MatSelect to find all maximal subsets
results <- MatSelect(cor_6var, threshold = 0.7, method = "els")
show(results)
#> CorrCombo object
#> -----------------
#>   Method:      els
#>   Threshold:   0.700
#>   Subsets:     3 maximal subsets
#>   Data Rows:   not applicable (matrix input)
#> 
#> Top combinations:
#>   No.  Variables                          Avg    Max    Size
#>   ------------------------------------------------------------
#>   [ 1] V1, V4, V5, V6                    0.375  0.650     4
#>   [ 2] V2, V4, V5, V6                    0.400  0.650     4
#>   [ 3] V3, V4, V5, V6                    0.425  0.650     4
```

**Interpreting the results**:

MatSelect identified three maximal subsets of size 4, exactly matching
the visual graph analysis. Each satisfies \\\|a\_{ij}\| \le 0.7\\ for
all pairs within the subset.

- **Subset 1** (V1, V4, V5, V6): Mean correlation 0.375, max 0.65

- **Subset 2** (V2, V4, V5, V6): Mean correlation 0.400, max 0.65

- **Subset 3** (V3, V4, V5, V6): Mean correlation 0.425, max 0.65

None can be extended: adding a second variable from V1-V3 would
introduce a pair with correlation above 0.7. The three differ only in
which block-1 variable they carry, and subset 1 has the lowest average
correlation. In practice, you might choose based on domain knowledge
(prefer variables with established theory) or downstream model
performance.

> **Key Points: Graph-Theoretic Interpretation**
>
> - Build threshold graph: nodes = variables, edges connect pairs with
>   \|a_ij\| \<= τ
> - Maximal valid subsets ↔︎ maximal cliques in threshold graph
> - Proven algorithms (ELS, Bron-Kerbosch) enumerate all maximal cliques
>   efficiently
> - Graph structure reveals variable clustering (densely connected =
>   similar variables)

------------------------------------------------------------------------

## From Theory to Implementation

The mathematical concepts defined earlier map directly onto the function
arguments and behavior of the package. The correspondence is outlined
below.

### **Threshold (\\\tau\\) → `threshold` argument**

Controls which edges appear in the threshold graph.

- `corrPrune(data, threshold = 0.7)` keeps an edge only if \\\|a\_{ij}\|
  \le 0.7\\

- Lower thresholds → stricter pruning → sparser graphs → smaller valid
  subsets

### **Maximal cliques → Returned subsets**

- [`corrSelect()`](https://gillescolling.com/corrselect/reference/corrSelect.md)
  returns **all** maximal cliques (full exact enumeration)

- [`corrPrune()`](https://gillescolling.com/corrselect/reference/corrPrune.md)
  returns **one** maximal clique (greedy or exact, depending on mode)

- Each clique corresponds exactly to a valid variable subset satisfying
  the threshold constraint

### **Forced-in set (\\F\\) → `force_in` argument**

Ensures that certain variables appear in every returned subset.

- `corrPrune(data, threshold = 0.7, force_in = c("age", "gender"))`

- Internally, the algorithm verifies that \\F\\ itself is a valid subset
  (all pairs in \\F\\ satisfy \\\|a\_{ij}\| \le \tau\\)

### **Search type → `mode` and `method` arguments**

- `mode = "exact"`\
  Enumerates all maximal cliques using ELS or Bron–Kerbosch
- `mode = "greedy"`\
  Constructs a single maximal clique via a greedy heuristic
- `mode = "auto"` Uses exact mode for \\p \le\\ `max_exact_p` (default
  100), greedy mode for larger \\p\\

Choice of enumeration algorithm:

- `method = "els"`\
  Eppstein–Löffler–Strash; recommended when `force_in` is specified
- `method = "bron-kerbosch"`\
  Bron–Kerbosch with pivoting (default)

### **Association matrix (\\A\\) → Data input and matrix-based functions**

How the association structure enters the algorithm:

- `corrPrune(data)`\
  Computes the correlation matrix internally and finds cliques
- `MatSelect(cor_matrix)`\
  Uses a precomputed association matrix directly
- `assocSelect(data)`\
  Computes mixed association measures (Pearson, eta-squared, Cramér’s V)
  before selection

### **Graph density → Performance considerations**

Depends directly on the threshold:

- Sparse graphs (low \\\tau\\)\
  Few edges → fast exact enumeration
- Dense graphs (high \\\tau\\)\
  Many edges → potentially exponential growth in maximal cliques

These properties motivate the `auto` mode: exact for small \\p\\, greedy
for larger \\p\\.

### **Example mapping**

Mathematical formulation:

\\ \text{Find all maximal } S \subseteq \\1,\dots,p\\ \text{ such that }
\|a\_{ij}\| \le 0.7\\ \forall i, j \in S,\\ \text{with } \\ \text{age}
\\ \subseteq S. \\

Implementation:

``` r

results <- corrSelect(
  data      = mydata,
  threshold = 0.7,      # τ = 0.7
  force_in  = "age",    # F = {age}
  mode      = "exact",  # enumerate all maximal cliques
  method    = "els"     # use ELS algorithm (recommended with force_in)
)
```

The returned object contains all maximal cliques, each representing a
valid variable subset that satisfies the threshold constraint and
includes the required variables.

> **Key Points: From Theory to Implementation**
>
> - `threshold` = τ (correlation cutoff)
> - `force_in` = F (variables required in all subsets)
> - `mode = "exact"` → enumerate all maximal cliques
> - `mode = "greedy"` → fast heuristic, single subset
> - `method = "els"` recommended when using `force_in`

------------------------------------------------------------------------

## Search Algorithms

### Exact Enumeration

Two algorithms enumerate all maximal cliques exactly:

#### Eppstein–Löffler–Strash (ELS)

Uses degeneracy ordering to structure the search:

1.  Compute degeneracy ordering \\v_1, \dots, v_p\\

2.  For each \\v_i\\, extend cliques within candidates \\\\v\_{i+1},
    \dots, v_p\\\\

3.  Recursively build cliques, pruning when no extension is possible

Formally, define:

\\ \text{extend}(R, P) = \begin{cases} \\R\\, & P = \emptyset \\
\bigcup\_{v \in P} \text{extend}(R \cup \\v\\, P \cap N(v)), &
\text{otherwise} \end{cases} \\

where \\N(v)\\ denotes neighbors of \\v\\ in \\G\\.

#### Bron–Kerbosch

Classical recursive backtracking with optional pivoting.

Let \\R\\ = current clique, \\P\\ = candidates, \\X\\ = excluded nodes:

\\ \text{BK}(R, P, X) = \begin{cases} \text{report}(R), & P = X =
\emptyset \\ \text{for each } v \in P \setminus N(u): \\ \quad
\text{BK}(R \cup \\v\\, P \cap N(v), X \cap N(v)) \\ \quad P \leftarrow
P \setminus \\v\\, \quad X \leftarrow X \cup \\v\\ \end{cases} \\

Pivot \\u \in P \cup X\\ reduces recursive calls.

#### Pseudocode for Practitioners

**Eppstein-Löffler-Strash (ELS) Algorithm**:

    Algorithm: ELS_MaxCliques(Graph G, ForceIn F)
    Input: Threshold graph G = (V, E), forced variables F ⊆ V
    Output: All maximal cliques containing F

    1. Validate F forms a clique in G

    2. Compute degeneracy ordering: v₁, v₂, ..., vₚ

    3. Initialize results = []

    4. For each vertex vᵢ in ordering:
       a. If vᵢ ∉ F and vᵢ not adjacent to all in F:
          Skip vᵢ (cannot extend F)

       b. candidates = {vⱼ : j > i and vⱼ adjacent to vᵢ} ∩ N(F)
       c. Extend(clique = {vᵢ} ∪ F, candidates, results)

    5. Return results

    Subroutine: Extend(R, P, results)
      If P is empty:
        Add R to results if maximal
        Return

      For each v in P:
        neighbors = P ∩ N(v)
        Extend(R ∪ {v}, neighbors, results)

**Bron-Kerbosch Algorithm**:

    Algorithm: BronKerbosch(Graph G, use_pivot = TRUE)
    Input: Threshold graph G = (V, E), pivot flag
    Output: All maximal cliques

    1. Initialize: R = ∅ (current clique)
                  P = V (candidates)
                  X = ∅ (excluded)
    2. Call BK(R, P, X, results)

    3. Return results

    Subroutine: BK(R, P, X, results)
      If P = ∅ and X = ∅:
        Add R to results (R is maximal)
        Return

      If use_pivot:
        Choose pivot u from P ∪ X with max |P ∩ N(u)|
        iterate = P \ N(u)  # Skip neighbors of pivot
      Else:
        iterate = P

      For each vertex v in iterate:
        BK(R ∪ {v},
           P ∩ N(v),     # Only candidates adjacent to v
           X ∩ N(v),     # Only excluded adjacent to v
           results)

        P = P \ {v}      # Remove v from candidates
        X = X ∪ {v}      # Add v to excluded

**Complexity**:

- **ELS**: \\O(d \cdot 3^{d/3})\\ where \\d\\ is degeneracy (much faster
  on sparse graphs)

- **Bron-Kerbosch**: \\O(3^{p/3})\\ worst case, improved with pivoting

- **Greedy** (below): \\O(p^2)\\ deterministic polynomial time

### Greedy Heuristic

Backward elimination: starts with all variables and iteratively removes
the “worst” one, in order of (1) most threshold violations, (2) highest
max association, (3) highest average association, (4) lowest column
index, until all remaining pairs satisfy \\\|a\_{ij}\| \le \tau\\.

Returns a single valid subset (not necessarily maximal or optimal).

Complexity: \\O(p^2)\\ vs \\O(3^{p/3})\\ for exact enumeration.

> **Key Points: Search Algorithms**
>
> - **Exact mode**: ELS or Bron-Kerbosch enumerate all maximal cliques
> - **ELS**: O(d · 3^{d/3}), faster on sparse graphs, better with
>   `force_in`
> - **Bron-Kerbosch**: O(3^{p/3}), pivoting improves performance
> - **Greedy**: O(p²), fast but returns single (possibly non-optimal)
>   subset
> - Rule of thumb: exact for p \<= `max_exact_p` (default 100), greedy
>   for p \> `max_exact_p`

------------------------------------------------------------------------

## Algorithm Pseudocode

### Eppstein–Löffler–Strash (ELS)

**Input**: Graph \\G = (V, E)\\, forced-in set \\F\\

**Output**: All maximal cliques containing \\F\\

    Algorithm ELS(G, F):
      # Step 1: Compute degeneracy ordering
      deg_order ← ComputeDegeneracyOrdering(G)

      # Step 2: Initialize with forced-in variables
      R ← F  (current clique)
      P ← V \ F  (candidate vertices)
      X ← ∅  (excluded vertices)

      # Step 3: Validate forced-in set
      for each i, j ∈ F:
        if (i, j) ∉ E:
          return ∅  (infeasible)

      # Step 4: Recursively enumerate maximal cliques
      return EnumerateCliques(R, P, X, deg_order)

    Subroutine EnumerateCliques(R, P, X, ordering):
      # Base case: no candidates, no exclusions → maximal clique
      if P = ∅ and X = ∅:
        report R as maximal clique
        return

      # Recursive case: extend with each candidate
      for each v ∈ P (in degeneracy order):
        # Extend clique
        R' ← R ∪ {v}

        # Update candidates: keep only neighbors of v
        P' ← P ∩ N(v)

        # Update exclusions: keep only neighbors of v
        X' ← X ∩ N(v)

        # Recurse
        EnumerateCliques(R', P', X', ordering)

        # Move v from candidates to exclusions
        P ← P \ {v}
        X ← X ∪ {v}

    Subroutine ComputeDegeneracyOrdering(G):
      # Degeneracy ordering: v_1, ..., v_p where each v_i has
      # minimum degree in G[{v_i, ..., v_p}]

      ordering ← [ ]
      remaining ← V

      while remaining ≠ ∅:
        # Find vertex with minimum degree in induced subgraph
        v ← argmin_{u ∈ remaining} |N(u) ∩ remaining|

        # Add to ordering (reverse order)
        ordering.prepend(v)
        remaining ← remaining \ {v}

      return ordering

**Complexity**: \\O(d \cdot 3^{d/3})\\ where \\d\\ is the degeneracy of
\\G\\.

**Properties:**

- **Exact:** enumerates all maximal cliques

- **Efficient on sparse graphs:** performs best when the threshold graph
  has low density

- **Forced-in support:** implemented by initializing the search with \\R
  = F\\

------------------------------------------------------------------------

### Bron–Kerbosch with Pivoting

**Input**: Graph \\G = (V, E)\\, forced-in set \\F\\

**Output**: All maximal cliques containing \\F\\

    Algorithm BronKerbosch(G, F):
      # Step 1: Initialize
      R ← F  (current clique)
      P ← V \ F  (candidate vertices)
      X ← ∅  (excluded vertices)

      # Step 2: Validate forced-in set
      for each i, j ∈ F:
        if (i, j) ∉ E:
          return ∅  (infeasible)

      # Step 3: Restrict candidates to neighbors of forced-in set
      if F ≠ ∅:
        P ← P ∩ (⋂_{v ∈ F} N(v))

      # Step 4: Enumerate
      return BK(R, P, X)

    Subroutine BK(R, P, X):
      # Base case: no candidates, no exclusions → maximal clique
      if P = ∅ and X = ∅:
        report R as maximal clique
        return

      # Choose pivot: vertex with most neighbors in P
      u ← argmax_{v ∈ P ∪ X} |N(v) ∩ P|

      # Iterate over non-neighbors of pivot (reduces recursion)
      for each v ∈ P \ N(u):
        # Extend clique
        R' ← R ∪ {v}

        # Update candidates: keep only neighbors of v
        P' ← P ∩ N(v)

        # Update exclusions: keep only neighbors of v
        X' ← X ∩ N(v)

        # Recurse
        BK(R', P', X')

        # Move v from candidates to exclusions
        P ← P \ {v}
        X ← X ∪ {v}

**Complexity**: \\O(3^{p/3})\\ maximal cliques (worst-case).

**Properties:**

- **Exact:** enumerates all maximal cliques

- **Pivoting reduces recursion:** fewer recursive calls and tighter
  branching

- **Classical algorithm (1973)**

**Pivot selection:**

- Choosing \\u\\ with maximum degree in \\P\\ minimizes branching

- Without pivoting: \\O(2^p)\\ recursive calls (exponential)

- With pivoting: \\O(3^{p/3})\\ recursive calls (still exponential but
  significantly tighter)

------------------------------------------------------------------------

### Greedy Heuristic

**Input**: Association matrix \\A\\, threshold \\\tau\\, forced-in set
\\F\\

**Output**: Single valid subset (not necessarily maximal)

    Algorithm GreedyPrune(A, τ, F):
      # Step 1: Initialize with all variables active
      S ← {1, ..., p}

      # Step 2: Validate forced-in set
      if F ≠ ∅:
        for each i, j ∈ F:
          if |a_ij| > τ:
            return ∅  (infeasible)

      # Step 3: Iteratively remove the worst violating variable
      while ∃ i, j ∈ S: |a_ij| > τ:
        # badness[v] = number of variables v currently violates the
        # threshold with; only variables with badness[v] > 0 are candidates
        for each v ∈ S:
          badness[v] ← |{u ∈ S : u ≠ v, |a_vu| > τ}|

        # Remove the worst candidate (see Tie-Breaking below), excluding F
        v_worst ← worst(S \ F, badness, A)
        S ← S \ {v_worst}

      # Step 4: Return pruned subset
      return S

**Complexity:** \\O(p^2 k)\\, where \\k\\ is the number of variables
removed.

**Properties:**

- **Fast:** polynomial-time procedure

- **Deterministic:** identical input always yields identical output

- **Non-optimal:** does not guarantee a maximal or largest subset

- **Forced-in support:** variables in \\F\\ are never removed

**Tie-breaking:** `worst()` selects the removed variable among
candidates with `badness[v] > 0`, in order:

1.  Highest `badness[v]` (most threshold violations)

2.  If still tied, highest max absolute association with any other
    active variable

3.  If still tied, highest average absolute association with all other
    active variables

4.  If still tied, lowest column index

------------------------------------------------------------------------

## Forced Variables

Constraint: variables \\F \subseteq \\1, \dots, p\\\\ must appear in all
returned subsets.

### Graph Modification

Modify the search:

- Require \\F \subseteq S\\ for all valid \\S\\

- Verify \\\|a\_{ij}\| \le \tau\\ for all \\i, j \in F\\ (else problem
  is infeasible)

- Search for maximal extensions of \\F\\ within remaining variables

Formally, find maximal cliques in \\G\\ containing \\F\\.

------------------------------------------------------------------------

## Correlation vs Association

**Correlation**: \\a\_{ij} \in \[-1, 1\]\\, use \\\|a\_{ij}\|\\ in
threshold constraint

**Association**: \\a\_{ij} \in \[0, 1\]\\, use \\a\_{ij}\\ directly

Mixed-type data uses association matrix with measures chosen per
variable-pair type.

------------------------------------------------------------------------

## Complexity Analysis

### Exact Enumeration

Worst-case: \\O(3^{p/3})\\ maximal cliques possible

Performance depends on graph density:

- Sparse (low \\\tau\\): fewer edges, faster enumeration

- Dense (high \\\tau\\): many edges, exponential growth

### Greedy Heuristic

Time: \\O(p^2 k)\\ where \\k\\ = iterations

Space: \\O(p^2)\\ for storing associations

Deterministic: same input produces same output

------------------------------------------------------------------------

## Output Structure

All selection functions return a `CorrCombo` object containing:

- `subset_list`: list of character vectors (variable names per subset)

- `avg_corr`: numeric vector (mean \\\|a\_{ij}\|\\ within each subset)

- `min_corr`: numeric vector (min \\\|a\_{ij}\|\\ within each subset)

- `max_corr`: numeric vector (max \\\|a\_{ij}\|\\ within each subset)

- `threshold`: value of \\\tau\\

- `forced_in`: forced variable names

- `cor_method`: correlation/association measure used

- `n_rows_used`: sample size after removing missing values

Results are sorted by:

1.  Subset size (descending)

2.  Average absolute association (ascending)

------------------------------------------------------------------------

## Design Philosophy

This section explains key design decisions underlying corrselect,
addressing common questions about why certain choices were made.

### Why “Maximal” Not “Maximum”?

**Maximal**: Cannot be extended further (locally optimal) **Maximum**:
Largest possible size (globally optimal)

corrselect enumerates **all maximal** subsets rather than just finding
the **single maximum** subset. Why?

1.  **Multiple equally good solutions**: Real datasets often have many
    maximal subsets of equal or similar size. Returning only one
    discards valuable information about alternative variable
    combinations.

2.  **Domain knowledge integration**: Users may prefer a slightly
    smaller subset containing specific variables over the globally
    largest subset. Having all options enables informed choice.

3.  **Sensitivity analysis**: Comparing multiple maximal subsets reveals
    structural properties of the correlation matrix (e.g., tight vs
    loosely connected clusters).

4.  **Computational feasibility**: Finding the maximum clique is
    NP-complete and often harder than enumerating all maximal cliques in
    practice. Modern maximal clique algorithms (ELS, Bron-Kerbosch) are
    highly efficient for typical correlation structures.

### Why Hard Threshold Not Soft Constraint?

corrselect enforces a hard threshold: \\\|a\_{ij}\| \le \tau\\ for all
pairs. Alternative approaches use soft constraints (penalty functions,
regularization). Why hard thresholds?

1.  **Interpretability**: “No pair exceeds \\\tau\\” is a clear,
    verifiable guarantee. Soft constraints produce solutions where some
    pairs may exceed \\\tau\\ with unknown magnitude.

2.  **Reproducibility**: Hard thresholds produce deterministic results.
    Soft constraints often require tuning parameters (penalty weights,
    convergence criteria) that affect reproducibility.

3.  **Domain-specific requirements**: Fields like ecological modeling
    have established thresholds (e.g., \\\tau = 0.7\\ for WorldClim
    variables) based on empirical evidence. Hard thresholds directly
    implement these guidelines.

4.  **Exact enumeration**: Hard constraints enable graph-theoretic
    formulation with exact algorithms. Soft constraints typically
    require heuristic optimization.

### Why Graph Algorithms Not Optimization?

corrselect uses specialized graph algorithms (ELS, Bron-Kerbosch) rather
than general optimization frameworks (integer programming,
metaheuristics). Why?

1.  **Asymptotic efficiency**: Graph algorithms exploit structural
    properties (sparsity, degeneracy) unavailable to generic solvers.
    For sparse graphs (low \\\tau\\), this yields orders-of-magnitude
    speedups.

2.  **Exact enumeration**: Graph algorithms guarantee finding all
    maximal cliques. Optimization approaches typically find one
    solution.

3.  **Forced variables**: Graph algorithms naturally handle forced-in
    constraints via initialization. Optimization approaches require
    additional constraints that may degrade performance.

4.  **Established theory**: Maximal clique enumeration has 50+ years of
    algorithmic development with proven complexity bounds and
    implementation strategies.

### Why Pairwise Associations Only?

corrselect considers only pairwise associations, not higher-order
interactions (partial correlations, conditional independence). Why?

1.  **Computational tractability**: Higher-order interactions require
    exponentially more computation. For \\p\\ variables, pairwise
    methods scale as \\O(p^2)\\, while \\k\\-way interactions scale as
    \\O(p^k)\\.

2.  **Clear interpretation**: “Variables \\i\\ and \\j\\ correlate at
    \\r = 0.85\\” is directly interpretable. Partial correlations
    require careful conditioning set selection and can be
    counterintuitive.

3.  **Robustness**: Pairwise correlations are stable with moderate
    sample sizes. Partial correlations require \\n \gg p\\ and are
    sensitive to model misspecification.

4.  **Method generality**: Pairwise associations work for any variable
    type (numeric, categorical, mixed). Higher-order methods often
    require strong distributional assumptions.

**When higher-order methods are appropriate**: If your goal is causal
discovery (identifying conditional independence structure), use methods
like PC algorithm or constraint-based causal inference. corrselect
focuses on reducing redundancy for predictive modeling and descriptive
analysis.

### Why Enumerate All Solutions Not Just Return One?

Many correlation-pruning tools (e.g., caret::findCorrelation) return a
single subset. corrselect can return all maximal subsets (exact mode) or
one (greedy mode). Why offer exhaustive enumeration?

1.  **Alternative solutions**: Multiple maximal subsets represent
    genuinely different variable combinations that all satisfy the
    threshold constraint. Arbitrarily choosing one discards information.

2.  **Downstream analysis**: Different subsets may be preferred for
    different models or research questions. Enumerating all options
    enables post-hoc selection criteria.

3.  **Documentation**: For reproducible research (especially JOSS/CRAN
    submissions), documenting all valid solutions provides transparency
    about algorithmic choices.

4.  **Computational cost**: For typical use cases (\\p \leq 30\\, \\\tau
    \geq 0.5\\), exhaustive enumeration completes in milliseconds. When
    infeasible, greedy mode provides a fast approximation.

**When to use greedy mode**: High-dimensional data (\\p \> 50\\), dense
correlation structure (high \\\tau\\), or when a single solution
suffices (e.g., automated pipelines).

> **Key Points: Design Philosophy**
>
> - **Maximal not maximum**: All locally optimal subsets enumerated, not
>   just the globally largest
> - **Hard threshold**: Simple, interpretable guarantee vs. soft penalty
>   optimization
> - **Graph algorithms**: Leverage decades of research; proven
>   correctness and complexity bounds
> - **Pairwise only**: Computational tractability, clear interpretation,
>   robustness
> - **Enumerate all**: Preserves information for downstream choice;
>   greedy available when speed critical

------------------------------------------------------------------------

## References

### Graph-Theoretic Algorithms

**Maximal clique enumeration**

- **Eppstein, D., Löffler, M., & Strash, D. (2010).**\
  *Listing all maximal cliques in sparse graphs in near-optimal time.*\
  Algorithms and Computation: ISAAC 2010, Lecture Notes in Computer
  Science 6506, 403–414.\
  [doi:10.1007/978-3-642-17517-6_36](https://doi.org/10.1007/978-3-642-17517-6_36)
  - **Foundation for the ELS algorithm:** degeneracy-based maximal
    clique enumeration

  - Complexity \\O(d \cdot 3^{d/3})\\, where \\d\\ is graph degeneracy

  - Used when `force_in` is specified
- **Bron, C., & Kerbosch, J. (1973).**\
  *Algorithm 457: Finding all cliques of an undirected graph.*\
  Communications of the ACM, 16(9), 575–577.\
  [doi:10.1145/362342.362367](https://doi.org/10.1145/362342.362367)
  - **Foundation for the Bron–Kerbosch algorithm:** classic backtracking
    with pivoting

  - Default exact enumeration method
- **Tomita, E., Tanaka, A., & Takahashi, H. (2006).**\
  *The worst-case time complexity for generating all maximal cliques and
  computational experiments.*\
  Theoretical Computer Science, 363(1), 28–42.\
  [doi:10.1016/j.tcs.2006.06.015](https://doi.org/10.1016/j.tcs.2006.06.015)
  - **Pivoting strategy:** refined pivot rules for Bron–Kerbosch

  - Establishes the \\O(3^{p/3})\\ worst-case bound

**Graph degeneracy**:

- Matula, D. W., & Beck, L. L. (1983). Smallest-last ordering and
  clustering and graph coloring algorithms. *Journal of the ACM*, 30(3),
  417-427.
  [doi:10.1145/2402.322385](https://doi.org/10.1145/2402.322385)

  - **Degeneracy ordering**: Fundamental concept for sparse graph
    algorithms

  - Used in ELS algorithm for efficient enumeration

**Independent sets and vertex cover**:

- Tsukiyama, S., Ide, M., Ariyoshi, H., & Shirakawa, I. (1977). A new
  algorithm for generating all the maximal independent sets. *SIAM
  Journal on Computing*, 6(3), 505-517.
  [doi:10.1137/0206036](https://doi.org/10.1137/0206036)

  - **Maximal independent set enumeration**: Theoretical foundation for
    threshold graph problem

  - Complements maximal clique enumeration (clique in complement graph)

### Multicollinearity and Variable Selection

**Variance inflation factor (VIF)**:

- Belsley, D. A., Kuh, E., & Welsch, R. E. (1980). *Regression
  Diagnostics: Identifying Influential Data and Sources of
  Collinearity*. Wiley.
  [doi:10.1002/0471725153](https://doi.org/10.1002/0471725153)

  - **Standard reference**: VIF computation and condition indices for
    collinearity diagnosis

  - Foundation for modelPrune() approach

- O’Brien, R. M. (2007). A caution regarding rules of thumb for variance
  inflation factors. *Quality & Quantity*, 41(5), 673-690.
  [doi:10.1007/s11135-006-9018-6](https://doi.org/10.1007/s11135-006-9018-6)

  - **Critical evaluation**: Common VIF thresholds (5, 10) may be
    inappropriate in some contexts

  - Recommends context-specific threshold selection

- Marquaridt, D. W. (1970). Generalized inverses, ridge regression,
  biased linear estimation, and nonlinear estimation. *Technometrics*,
  12(3), 591-612. [doi:10.2307/1267205](https://doi.org/10.2307/1267205)

  - **Ridge regression**: Alternative approach to handling
    multicollinearity via regularization

  - Contrasts with hard variable removal (corrselect approach)

**Correlation-based variable selection**:

- Guyon, I., & Elisseeff, A. (2003). An introduction to variable and
  feature selection. *Journal of Machine Learning Research*, 3,
  1157-1182.

  - **Survey**: Overview of filter, wrapper, and embedded methods for
    variable selection

  - Context for correlation-based filtering (filter method)

- Hall, M. A. (1999). Correlation-based feature selection for machine
  learning. PhD thesis, University of Waikato.

  - **Correlation-based feature selection (CFS)**: Alternative criterion
    combining feature-class correlation and feature-feature correlation

  - Differs from corrselect’s pairwise-only approach

### Association Measures

**Numeric associations**:

- Pearson, K. (1895). Notes on regression and inheritance in the case of
  two parents. *Proceedings of the Royal Society of London*, 58,
  240-242.

  - **Pearson correlation**: Standard linear association measure

- Spearman, C. (1904). The proof and measurement of association between
  two things. *American Journal of Psychology*, 15(1), 72-101.
  [doi:10.2307/1412159](https://doi.org/10.2307/1412159)

  - **Spearman’s rank correlation**: Monotonic association for ordered
    data

- Kendall, M. G. (1938). A new measure of rank correlation.
  *Biometrika*, 30(1/2), 81-93.
  [doi:10.2307/2332226](https://doi.org/10.2307/2332226)

  - **Kendall’s tau**: Alternative rank-based measure, robust to
    outliers

**Categorical associations**:

- Cramér, H. (1946). *Mathematical Methods of Statistics*. Princeton
  University Press.

  - **Cramér’s V**: Chi-squared-based association for categorical
    variables

  - Used in assocSelect() for factor-factor pairs

- Pearson, K. (1900). On the criterion that a given system of deviations
  from the probable in the case of a correlated system of variables is
  such that it can be reasonably supposed to have arisen from random
  sampling. *Philosophical Magazine*, 50(302), 157-175.

  - **Chi-squared test**: Foundation for Cramér’s V

**Mixed-type associations**:

- Kelley, T. L. (1935). An unbiased correlation ratio measure.
  *Proceedings of the National Academy of Sciences*, 21(9), 554-559.

  - **Eta-squared (correlation ratio)**: Association between numeric and
    categorical variables

  - Used in assocSelect() for numeric-factor pairs

### Threshold Graph Theory

- Mahadev, N. V. R., & Peled, U. N. (1995). *Threshold Graphs and
  Related Topics*. Amsterdam: Elsevier, Annals of Discrete Mathematics,
  Vol. 56.
  [doi:10.1016/S0167-5060(13)71063-X](https://doi.org/10.1016/S0167-5060(13)71063-X)

  - **Comprehensive reference**: Mathematical properties of threshold
    graphs

  - Theoretical foundation for threshold-based graph construction

- Chvátal, V., & Hammer, P. L. (1977). Aggregation of inequalities in
  integer programming. *Annals of Discrete Mathematics*, 1, 145-162.

  - **Threshold graph characterization**: Alternative definitions and
    recognition algorithms

### Computational Complexity

- Garey, M. R., & Johnson, D. S. (1979). *Computers and Intractability:
  A Guide to the Theory of NP-Completeness*. W.H. Freeman.

  - **Standard reference**: Complexity classes, NP-completeness

  - Context for understanding exponential enumeration complexity

- Moon, J. W., & Moser, L. (1965). On cliques in graphs. *Israel Journal
  of Mathematics*, 3(1), 23-28.
  [doi:10.1007/BF02760024](https://doi.org/10.1007/BF02760024)

  - **Theoretical bound**: Proof that graphs can have up to \\3^{p/3}\\
    maximal cliques

  - Justifies worst-case complexity for exact enumeration

### Applications

**Bioclimatic modeling**:

- Dormann, C. F., et al. (2013). Collinearity: a review of methods to
  deal with it and a simulation study evaluating their performance.
  *Ecography*, 36(1), 27-46.
  [doi:10.1111/j.1600-0587.2012.07348.x](https://doi.org/10.1111/j.1600-0587.2012.07348.x)

  - **Species distribution models**: Evaluation of multicollinearity
    approaches in ecology

  - Recommends threshold of \\\tau = 0.7\\ for bioclimatic variables

**Genomics**:

- Saeys, Y., Inza, I., & Larrañaga, P. (2007). A review of feature
  selection techniques in bioinformatics. *Bioinformatics*, 23(19),
  2507-2517.
  [doi:10.1093/bioinformatics/btm344](https://doi.org/10.1093/bioinformatics/btm344)

  - **High-dimensional biology**: Variable selection in genomics and
    proteomics

  - Context for correlation-based filtering in gene expression analysis

------------------------------------------------------------------------

## See Also

- [`vignette("quickstart")`](https://gillescolling.com/corrselect/articles/quickstart.md) -
  Interface overview and examples

- [`vignette("workflows")`](https://gillescolling.com/corrselect/articles/workflows.md) -
  Real-world workflow examples

- [`vignette("advanced")`](https://gillescolling.com/corrselect/articles/advanced.md) -
  Algorithmic control and custom engines

- [`vignette("comparison")`](https://gillescolling.com/corrselect/articles/comparison.md) -
  Comparison with alternatives

## Session Info

``` r

sessionInfo()
#> R version 4.6.0 (2026-04-24 ucrt)
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
#> [1] igraph_2.3.1     corrselect_3.2.3
#> 
#> loaded via a namespace (and not attached):
#>  [1] svglite_2.2.2     cli_3.6.6         knitr_1.51        rlang_1.2.0      
#>  [5] xfun_0.57         otel_0.2.0        textshaping_1.0.5 S7_0.2.2         
#>  [9] jsonlite_2.0.0    htmltools_0.5.9   sass_0.4.10       rmarkdown_2.31   
#> [13] evaluate_1.0.5    jquerylib_0.1.4   fastmap_1.2.0     yaml_2.3.12      
#> [17] lifecycle_1.0.5   compiler_4.6.0    fs_2.1.0          pkgconfig_2.0.3  
#> [21] htmlwidgets_1.6.4 Rcpp_1.1.1-1.1    systemfonts_1.3.2 digest_0.6.39    
#> [25] R6_2.6.1          magrittr_2.0.5    bslib_0.11.0      tools_4.6.0      
#> [29] pkgdown_2.2.0     cachem_1.1.0      desc_1.4.3
```

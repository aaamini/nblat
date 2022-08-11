#' @import igraph Matrix
get_adj = function(g) {
  A = as_adj(g)
  if (!is_directed(g)) {
    A = forceSymmetric(A)
  }
  A
}

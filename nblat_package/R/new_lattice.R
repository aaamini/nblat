
#### Oracle counter setup
nblat_env = new.env()
nblat_env$oracle_counter = 0L # global variable to keep track of the number of oracle calls

reset_oracle_counter = function() {
  nblat_env$oracle_counter = 0L
}

inc_oracle_counter = function() {
  # cat('.')
  nblat_env$oracle_counter = nblat_env$oracle_counter + 1
}
####

#' @export
nblat <- function(j, S, Sig, restricted_set=NULL, should_sort=T) {
  all_nodes = 1:nrow(Sig)
  supp <- function(c) all_nodes[as.vector(as(c,"nMatrix"))]

  get_mb <- function(j, S) {
    c <- getParentCoefs2(j, S, Sig, threshold=T)$coefs
    supp(c)
  }

  ci_oracle = function(j, k, m) {
    inc_oracle_counter()
    c = getParentCoefs2(j, union(k, m), Sig, threshold=T)$coefs
    if (c[k] == 0) {
      return(TRUE)
    }
    return(FALSE)
  }

  if (is.null(restricted_set))  {
    restricted_set <- all_nodes
  }

  aux <- setdiff(restricted_set, c(j,S))
  M <- S
  m <- get_mb(j, S)
  for (k in aux) {
    if (ci_oracle(j, k, m)) M <- union(k,M)
    # propose <- union(k,M)
    # if ( setequal(get_mb(j, propose), m) ) {
    #    M <- propose
    # }
  }

  sort_or_not <- function(x) if (should_sort) sort(x) else x
  m=sort_or_not(m)
  M=sort_or_not(M)
  S=sort_or_not(S)

  lattice <- list(m = as_set(m), M = as_set(M), num_elms = 2^length(setdiff(M,m)), S = as_set(S))
  class(lattice) <- "nbhdLattice"
  return ( lattice )
}

#' @export
nblat_decomp <- function(j, Sig, should_sort=T) {
  all_nodes = 1:nrow(Sig)
  n <- length(all_nodes[-j])

  reset_oracle_counter()
  cat(sprintf('Oracle counter is = %d\n', nblat_env$oracle_counter))

  lats <- list()
  lats[[1]] <- nblat(j, all_nodes[-j], Sig, should_sort=should_sort)
  Mm_diff <- length(lats[[1]]$M) - length(lats[[1]]$m)
  PO_count <- Mm_diff * 2^(Mm_diff-1)
  K <- 1
  total_num_covered <- lats[[1]]$num_elms
  while (TRUE) {
    S <- find_uncovered_set(lats, all_nodes[-j])
    if (length(S) == 0) break
    K <- K+1
    lats[[K]] <- nblat(j, S, Sig, should_sort=should_sort)
    total_num_covered <- total_num_covered + lats[[K]]$num_elms
    Mm_diff <- length(lats[[K]]$M) - length(lats[[K]]$m)
    PO_count <- PO_count + Mm_diff * 2^(Mm_diff-1)
    if (K %% 5 == 0) {
      cat("\r", total_num_covered, " out of ", 2^n,
          " (", round(total_num_covered / 2^n * 100,2), "%) covered.")
    }
    #print(lats[[K+1]])
    #cat('\n')
  }
  cat("\r", total_num_covered, " out of ", 2^n,
      " (", round(total_num_covered / 2^n * 100,2), "%) covered.")

  list(lats=lats, PO_count=PO_count,
       n_oracle_calls = nblat_env$oracle_counter)
}


# Not very efficient to construct the problem every time using CVX, but should do for now
# Might be better to call the SCS solver directly with the data.
# get_problem_data(prob, 'SCS')$data
#' @import CVXR
find_epsmax = function(Del) {
  d = nrow(Del)
  eps = Variable(1)
  S = Variable(d, d, PSD =T)

  con = list(S == Del*eps + diag(1, d))
  prob = Problem(Maximize(eps), con)
  res = solve(prob)
  res$value
}

normalize_infnorm = function(x) x / max(abs(x))

#' @import Matrix
sample_Del = function(G) {
  sG = summary(G)
  g = length(sG$i)
  Del_values = normalize_infnorm(runif(g, min = -1, max = 1))
  Del = sparseMatrix(i = sG$i, j = sG$j, x = Del_values, symmetric = T)
  as(Del, "dgCMatrix")
}
make_prec_mat = function(eps, Del) {
  A = eps*Del
  diag(A) = 1
  A
}

# The main point of the code -- samples from G-Markov precision matrices with
# unit diagonal entries, solves and SDP to find epsmax
rprec1 =  function(G) {
# sample_prec_mat = function(G) {
  Del = sample_Del(G)
  epsmax = find_epsmax(Del)
  eps = runif(1, min = 0, max = epsmax)
  Gam = make_prec_mat(eps, Del)
  list(Gam = Gam, Del = Del, eps = eps, epsmax = epsmax)
}

# Does the same thing by rejection sampling
#' @export
rprec2 <- function(A) {
# random_invcov <- function(A) {
  #A[upper.tri(A)] <- 0
  #A[as.matrix((abs(A) > 0) & lower.tri(A))] <- rnorm(nnzero(A))
  #A <- drop0(A)
  #A <- A + t(A)
  d = nrow(A)
  non_psd <- TRUE
  while (non_psd) {
    eps <- runif(1)
    Gamma <- diag(1,d) + eps*A
    eig_vals <- eigen(Gamma)$values
    non_psd <- min(eig_vals) < 0
    cat('.')
  }
  list(Gamma=Gamma, eps=eps)
}


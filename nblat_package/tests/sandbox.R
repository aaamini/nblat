library(Matrix)
set.seed(15)
d = 10
G = as(rsparsematrix(d, d, density = 0.2, symmetric = T), "nsparseMatrix")
diag(G) = F
image(G)

out = sample_prec_mat(G)
Sig = solve(out$Gam)

library(igraph)
pcg = read_graph("../R_code/simple_pcg","graphml")
usethis::use_data(pcg, overwrite = T)
load("../R_code/Sigma.rda")
usethis::use_data(Sig, overwrite = T)

# A = get_adj(pcg)
# Gam = rprec2(A)
# Sig = solve(Gam)
Sig
all_nodes = 1:nrow(Sig)
j <- 3
S_list = list(
  all_nodes[-j],
  c(2,6,12),
  c(4,6,12,14),
  c(7,10),
  c(8,9,11,12,14))

for (S in S_list) {
  print(computeLattice(j, S, Sig, all_nodes, should_sort=T))
  cat('\n')
}

# use_test("lattice")

computeLattice(j, S, Sig, all_nodes, should_sort=T)
Sig
S
all_nodes

j <- 3
out <- nblat_decomp(j, Sig, should_sort=T)
lats <- out$lats
(PO_count <- out$PO_count)
out$n_oracle_calls

(set_counts <- table(unlist(lapply(lats, function(lat) lat$num_elms))))
library(xtable)
xtable(t(set_counts))


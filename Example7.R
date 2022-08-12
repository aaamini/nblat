library(nblat)
Sig # Example covariance matrix, exists as a dataset in the package

all_nodes = 1:nrow(Sig)
j <- 3
S_list = list(
  all_nodes[-j],
  c(2,6,12),
  c(4,6,12,14),
  c(7,10),
  c(8,9,11,12,14))

for (S in S_list) {
  print(nblat(j, S, Sig))
  cat('\n')
}

library(nblat)

Gam1 = matrix(1,4,4)
diag(Gam1) = 3
Gam1[4+cumsum(c(0,3,3,3))] = 0
Gam2 = matrix(c(4,3,-3,3,5,-3,-3,-3,3),3)
Gam = Matrix::bdiag(Gam1, Gam2)
Sigma = solve(Gam)

j = 5
S = 6
nblat(j, S, Sigma)

# nblat_decomp(j, Sigma)

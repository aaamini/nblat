library(nblat)
j <- 3
out <- nblat_decomp(j, Sig)
lats <- out$lats
out$PO_count
out$n_oracle_calls

(set_counts <- table(unlist(lapply(lats, function(lat) lat$num_elms))))
# xtable::xtable(t(set_counts))

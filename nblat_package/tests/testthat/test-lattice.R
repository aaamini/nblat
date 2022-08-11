test_that("lattice computation works", {
  all_nodes = 1:nrow(Sig)
  j <- 3
  S = c(2,6,12)
  # lat = computeLattice(j, S, Sig, all_nodes, should_sort=T)
  lat = nblat(j, S, Sig)
  expect_equal(lat$m, as_set(c(2,6,12)))
  expect_equal(lat$M, as_set(c(1,2,4,5,6,7,8,9,10,11,12,13,14,15)))

  S = c(8,9,11,12,14)
  # lat = computeLattice(j, S, Sig, all_nodes, should_sort=T)
  lat = nblat(j, S, Sig)
  expect_equal(lat$m, as_set(c(9,12,14)))
  expect_equal(lat$M, as_set(c(4,5,7,8,9,10,11,12,13,14,15)))

  S = c(4,6,12,14)
  lat = nblat(j, S, Sig)
  expect_equal(lat$m, as_set(c(6,12)))
  expect_equal(lat$M, as_set(c(4,5,6,7,10,11,12,14,15)))

  S = c(7,10)
  lat = nblat(j, S, Sig)
  expect_equal(lat$m, empty_set())
  expect_equal(lat$M, as_set(c(7,10)))
})

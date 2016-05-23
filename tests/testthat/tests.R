context("largeVis")

test_that("Can determine iris neighbors", {
  data (iris)
  RcppArmadillo::armadillo_set_seed(1974)
  set.seed(1974)
  dat <- as.matrix(iris[, 1:4])
  dat <- scale(dat)
  dupes <- which(duplicated(dat))
  dat <- dat[-dupes, ]
  dat <- t(dat)
  neighbors <- randomProjectionTreeSearch(dat, K = 5, n_trees = 10, tree_threshold = 20, max_iter = 10,
                                          verbose = FALSE)
  expect_equal(nrow(neighbors), 5)
  expect_equal(ncol(neighbors), ncol(dat))
  expect_equal(sum(neighbors == -1), 0)
  expect_equal(sum(neighbors[, 1:40] > 50), 0)
})

test_that("Can determine iris neighbors accurately", {
  M <- 5
  RcppArmadillo::armadillo_set_seed(1974)
  set.seed(1974)
  data (iris)
  dat <- as.matrix(iris[, 1:4])
  dat <- scale(dat)
  dupes <- which(duplicated(dat))
  dat <- dat[-dupes, ]
  d_matrix = as.matrix(dist(dat, method = 'euclidean'))
  bests <- apply(d_matrix, MARGIN=1, FUN = function(x) order(x)[1:(M + 1)])
  bests <- bests[-1,] - 1
  dat <- t(dat)
  neighbors <- randomProjectionTreeSearch(dat, K = M, n_trees = 5, tree_threshold = 10, max_iter = 1,
                                          verbose = FALSE)
  scores <- lapply(1:ncol(dat), FUN = function(x) sum(neighbors[,x] %in% bests[,x]))
  score <- sum(as.numeric(scores))
  expect_equal(score, ncol(dat) * M)
})

test_that("largeVis works", {
  RcppArmadillo::armadillo_set_seed(1974)
  set.seed(1974)
  data(iris)
  dat <- as.matrix(iris[, 1:4])
  dat <- scale(dat)
  dupes <- which(duplicated(dat))
  dat <- dat[-dupes, ]
  dat <- t(dat)
  visObject <- vis(dat, max_iter = 20, sgd_batches = 1000,
                        K = 10,  gamma = 0.5, verbose = FALSE)
  expect_equal(sum(any(is.na(visObject$coords)) + any(is.nan(visObject$coords)) + any(is.infinite(visObject$coords))), 0)
})

test_that("largeVis works without weights", {
  RcppArmadillo::armadillo_set_seed(1974)
  set.seed(1974)
  data(iris)
  dat <- as.matrix(iris[, 1:4])
  dat <- scale(dat)
  dupes <- which(duplicated(dat))
  dat <- dat[-dupes, ]
  dat <- t(dat)
  visObject <- vis(dat, max_iter = 20, sgd_batches = 1000, weight_pos_samples = FALSE,
                   K = 10, verbose = FALSE)
  expect_equal(sum(any(is.na(visObject$coords)) + any(is.nan(visObject$coords)) + any(is.infinite(visObject$coords))), 0)
})

test_that("largeVis works with cosine", {
  RcppArmadillo::armadillo_set_seed(1974)
  set.seed(1974)
  data(iris)
  dat <- as.matrix(iris[, 1:4])
  dat <- scale(dat)
  dupes <- which(duplicated(dat))
  dat <- dat[-dupes, ]
  dat <- t(dat)
  visObject <- vis(dat, max_iter = 20, sgd_batches = 1000, weight_pos_samples = FALSE,
                   K = 10, verbose = FALSE, distance_method="Cosine")
  expect_equal(sum(any(is.na(visObject$coords)) + any(is.nan(visObject$coords)) + any(is.infinite(visObject$coords))), 0)
})


test_that("largeVis works when alpha == 0", {
  RcppArmadillo::armadillo_set_seed(1974)
  set.seed(1974)
  data(iris)
  dat <- as.matrix(iris[, 1:4])
  dat <- scale(dat)
  dupes <- which(duplicated(dat))
  dat <- dat[-dupes, ]
  dat <- t(dat)
  visObject <- vis(dat, max_iter = 20, sgd_batches = 10000,
                   K = 10,  alpha = 0, verbose = FALSE, weight_pos_samples = FALSE)
  expect_equal(sum(any(is.na(visObject$coords)) + any(is.nan(visObject$coords)) + any(is.infinite(visObject$coords))), 0)
})

test_that("prepare_binomial_response works", {
  y1 <- prepare_binomial_response(1:2)
  y2 <- prepare_binomial_response(2:1)
  y3 <- prepare_binomial_response(c(TRUE, FALSE))
  y4 <- prepare_binomial_response(factor(1:2))
  y5 <- prepare_binomial_response(factor(2:1))
  y6 <- prepare_binomial_response(c("N", "Y"))
  y7 <- prepare_binomial_response(factor(c("N", "Y")))
  y8 <- prepare_binomial_response(factor(c("N", "Y"), levels = c("Y", "N"), ordered = TRUE))

  t1 <- c(TRUE, FALSE)
  t2 <- c(FALSE, TRUE)
  expect_equal(as.vector(y1), t2)
  expect_equal(as.vector(y2), t1)
  expect_equal(as.vector(y3), t1)
  expect_equal(as.vector(y4), t2)
  expect_equal(as.vector(y5), t1)
  expect_equal(as.vector(y6), t2)
  expect_equal(as.vector(y7), t2)
  expect_equal(as.vector(y8), t1)
  expect_equal(as.vector(prepare_binomial_response(c(2, 1, 2))), c(TRUE, FALSE, TRUE))
})

tb <- tibble::tibble(group = c(rep("a", 25), rep("b", 15), rep("c", 10)),
                     outcome = c(rep(0L, 20), rep(1L, 15), rep(0L, 10), rep(1L, 5))) %>%
  dplyr::mutate(groupf = factor(group)) %>%
  dplyr::mutate(outf = factor(outcome)) %>%
  dplyr::mutate(outb = as.logical(outcome)) %>%
  dplyr::mutate(outbf = factor(outb)) %>%
  dplyr::mutate(outc = ifelse(outb, "yes", "no"))
ref <- suppressMessages(response_binom(tb, "outcome", "group", plot = FALSE))

test_that("response_binom works", {
  expect_s3_class(ref, "data.frame")
  #  expect_s3_class(suppressMessages(response_binom(tb, "outcome", "group", return_plot = TRUE)), "ggplot")
  expect_identical(nrow(ref), length(unique(tb$group)))
  expect_true(all(ref$prop >= 0 & ref$prop <= 1))
  expect_true(all(ref$lo <= ref$prop))
  expect_true(all(ref$hi >= ref$prop))
})

test_that("response_binom handles inputs", {
  #ignore attribute differences here
  attr(ref, "waldo_opts") <- list(ignore_attr = TRUE)
  expect_identical(suppressMessages(response_binom(tb, "outf", "group", plot = FALSE)), ref)
  expect_identical(suppressMessages(response_binom(tb, "outb", "group", plot = FALSE)), ref)
  expect_identical(suppressMessages(response_binom(tb, "outbf", "group", pos_class = "TRUE", plot = FALSE)), ref)
  expect_identical(suppressMessages(response_binom(tb, "outc", "group", pos_class = "yes", plot = FALSE)), ref)
  expect_false(identical(suppressMessages(response_binom(tb, "outc", "group", plot = FALSE)), ref))
  expect_message(response_binom(tb, "outc", "group", plot = FALSE), "Treating")
  # next tests predictor var as factor. Not identical since "value" in returned df is factor
  #expect_identical(response_binom(tb, "outcome", "groupf"), ref)
  tb2 <- tibble::add_row(tb, group = "c", outcome = 2)
  expect_error(suppressMessages(response_binom(tb2, "outcome", "group", plot = FALSE)), "Target variable must be binary.")
})

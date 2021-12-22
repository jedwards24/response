test_that("prepare_binomial_response() works", {
  y1 <- prepare_binomial_response(1:2)
  y2 <- prepare_binomial_response(2:1)
  y3 <- prepare_binomial_response(c(TRUE, FALSE))
  y4 <- prepare_binomial_response(factor(1:2))
  y5 <- prepare_binomial_response(factor(2:1))
  y6 <- prepare_binomial_response(c("N", "Y"))
  y7 <- prepare_binomial_response(factor(c("N", "Y")))
  y8 <- prepare_binomial_response(factor(c("N", "Y"), levels = c("Y", "N"), ordered = TRUE))
  y9 <- prepare_binomial_response(1:2, pos_class = 1L)

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
  expect_equal(as.vector(y8), t1)
  expect_equal(as.vector(prepare_binomial_response(c(2, 1, 2))), c(TRUE, FALSE, TRUE))
  expect_error(prepare_binomial_response(1:2, pos_class = 3L))
  expect_error(prepare_binomial_response(factor(5:6), pos_class = 3L))
})

tb <- tibble::tibble(group = c(rep("a", 25), rep("b", 15), rep("c", 10)),
                     outcome = c(rep(0L, 20), rep(1L, 15), rep(0L, 10), rep(1L, 5))) %>%
  dplyr::mutate(groupf = factor(group)) %>%
  dplyr::mutate(outf = factor(outcome)) %>%
  dplyr::mutate(outb = as.logical(outcome)) %>%
  dplyr::mutate(outbf = factor(outb)) %>%
  dplyr::mutate(outc = ifelse(outb, "yes", "no"))
ref <- suppressMessages(response(tb, "outcome", "group", plot = FALSE))

test_that("binom works", {
  expect_s3_class(ref, "data.frame")
  #  expect_s3_class(suppressMessages(response(tb, "outcome", "group", return_plot = TRUE)), "ggplot")
  expect_identical(nrow(ref), length(unique(tb$group)))
  expect_true(all(ref$prop >= 0 & ref$prop <= 1))
  expect_true(all(ref$lo <= ref$prop))
  expect_true(all(ref$hi >= ref$prop))
})

test_that("response() handles inputs", {
  #ignore attribute differences here
  attr(ref, "waldo_opts") <- list(ignore_attr = TRUE)
  expect_identical(suppressMessages(response(tb, "outf", "group", plot = FALSE)), ref)
  expect_identical(suppressMessages(response(tb, "outb", "group", plot = FALSE)), ref)
  expect_identical(suppressMessages(response(tb, "outbf", "group", pos_class = "TRUE", plot = FALSE)), ref)
  expect_identical(suppressMessages(response(tb, "outc", "group", pos_class = "yes", plot = FALSE)), ref)
  expect_false(identical(suppressMessages(response(tb, "outc", "group", plot = FALSE)), ref))
  expect_message(response(tb, "outc", "group", plot = FALSE), "Treating.*value for positive class.")
  # next tests predictor var as factor. Not identical since "value" in returned df is factor
  #expect_identical(response(tb, "outcome", "groupf"), ref)
  tb2 <- tibble::add_row(tb, group = "c", outcome = 2)
  expect_error(suppressMessages(response(tb2, "outcome", "group", plot = FALSE)), "Target variable must be binary.")
})

test_that("response() output matches snapshot", {
  set.seed(13)
  a <- rbinom(30, 1, 0.1)
  b <- rbinom(55, 1, 0.25)
  c <- rbinom(15, 1, 0.7)
  df <- data.frame(x = c(rep("A", 30), rep("B", 55), rep("C", 15)),
                   y = c(a, b, c))
  tt <- response(df, y, x, pos_class = 1L, plot = FALSE)
  expect_true(all(c("pos_class_supplied", "response_classes", "mean_all") %in% names(attributes(tt))))
  expect_named(attributes(tt),
               c("pos_class_supplied", "response_classes", "mean_all", "class",
                 "row.names", "names", "target_name", "grouping_name"),
               ignore.order = TRUE)
  expect_snapshot_value(tt, style = "json2")
  expect_identical(response(df, y, x, pos_class = 1L, plot = FALSE, order_n = FALSE)[[1]],
                   c("A", "B", "C"))
})

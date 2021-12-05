
#prepare_binomial_response() -------
# Decisions:
#   - default to order in vector or sort() order.
#   - Does factor level order matter? What if ordered factor?
# Converting to integer is a bit tricky. Just convert to logical based on match to pos_class instead?
x <- c(1, 2, 2, 1)
x <- c(T, F, T)
x <- factor(c(T, F, T))
x <- c("N", "Y", "Y")
xlist <- list(factor(c("N", "Y", "Y")),
               c(1, 2, 2, 1),
               c(T, F, T),
               factor(c(T, F, T)),
               c("N", "Y", "Y"),
               factor(c("N", "Y", "Y"))
)
x2 <- prepare_binomial_response(c(1, 0, 1))
all.equal(x2, c(T, F, T))
as.vector(x2)
testthat::expect_equal(as.vector(x2), c(T, F, T))
testthat::expect_true(all(x2 == c(T, F, T)))

attr(x, "bbb") <- "sale"
x$sale
  ?as.vector
purrr::map(xlist, ~tibble::tibble(., out = prepare_binomial_response2(.)))
prepare_binomial_response2(c(1, 0, 1))
labels(x)
levels(x)
debugonce(prepare_binomial_response)
f <- factor(3:5)
f
as.vector(f)

1 == TRUE
1 == 1L
identical(1, T)

x <- factor(5:3, levels = 5:3, ordered = T)
as.vector(x)


as.numeric(x)

x <- factor(3:5)
as.numeric(x)
as.vector(x)
as.integer(as.vector(x))
as.integer(letters[1:3])


x <- c("a", NA, "(Missing)")
forcats::fct_explicit_na(x)
is.numeric(x)


#from edwards-----------


# response_binom rough from edwards ------------

library(tidyverse)
dt <- diamonds %>%
  sample_n(1000) %>%
  mutate(best = cut == "Ideal")
count(dt, best)
#count_over(dt, cut, color)
dt[2, 4] <- NA
#dt[3, 4] <- "(Missing)"
dt <- mutate(dt, clarity = fct_recode(clarity, "(Missing)" = "I1"))
count(dt, clarity)
response_binom(dt, "best", "clarity", min_n = 0)
response_binom(dt, "best", "clarity", min_n = 0, plot = F)
res <- response_binom(dt, "best", "clarity", min_n = 0)
gg <- plot_response(res)
gg + ylab("Mean Proportion `best`")

response_binom(dt, "best", "clarity", min_n = 0, order_n = T)
expect_s3_class(gg, "ggplot")

dt
response_binom(dt, "best", "clarity", min_n = 0)
response_binom(dt, "best", "clarity", min_n = 60)
response_binom(dt, "best", "clarity", min_n = 300)
response_binom(dt, "best", "color", min_n = 0)

response_binom(dt, "best", "table", min_n = 10, order_n = F)
response_binom(dt, "best", "table", min_n = 10, order_n = T)
response_binom(dt, "best", "table", min_n = 10)

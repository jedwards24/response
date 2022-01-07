
# binomial data set ---------

set.seed(13)
a <- rbinom(30, 1, 0.1)
b <- rbinom(55, 1, 0.25)
c <- rbinom(15, 1, 0.7)


df <- data.frame(x = c(rep("A", 30), rep("B", 55), rep("C", 15)),
             y = c(a, b, c))
tibble::as_tibble(df)
x <- response(df, y, x, pos_class = 1)
attributes(x)
response(df, y, x, pos_class = 1, order_n = T)
response(df, y, x, family = "gaussian")

# response() rough from edwards ------------

library(tidyverse)
set.seed(6)
dt <- diamonds %>%
  sample_n(1000) %>%
  mutate(best = cut == "Ideal")
count(dt, best)
#count_over(dt, cut, color)
dt[2, 4] <- NA
response(dt, "best", "clarity", min_n = 0, pos_class = TRUE)
response(dt, "best", "clarity", min_n = 0, pos_class = FALSE)

#dt[3, 4] <- "(Missing)"
dt <- mutate(dt, clarity = fct_recode(clarity, "(Missing)" = "I1"))
count(dt, clarity)
response(dt, "best", "clarity", min_n = 0)
response(dt, "best", "clarity", min_n = 0, plot = F)
res <- response(dt, "best", "clarity", min_n = 0)
gg <- plot_response(res)
gg + ylab("Mean Proportion `best`")

response(dt, "best", "clarity", min_n = 0, order_n = T)
expect_s3_class(gg, "ggplot")

dt
response(dt, "best", "clarity", min_n = 0)
response(dt, "best", "clarity", min_n = 60)
response(dt, "best", "clarity", min_n = 300)
response(dt, "best", "color", min_n = 0)

response(dt, "best", "table", min_n = 10, order_n = F)
response(dt, "best", "table", min_n = 10, order_n = T)
response(dt, "best", "table", min_n = 10)

#--------------
set.seed(13)
a <- rbinom(30, 1, 0.1)
b <- rbinom(55, 1, 0.25)
c <- rbinom(15, 1, 0.7)
df <- data.frame(x = c(rep("A", 30), rep("B", 55), rep("C", 15)),
                 y = c(a, b, c))
tt <- response(df, y, x, pos_class = 1L, plot = FALSE)
plot_response(tt)
tt
attributes(tt)
str(tt)
deparse(tt)
jsonlite::serializeJSON(tt)
jsonlite::fromJSON(tt)

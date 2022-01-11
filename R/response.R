#' Confidence intervals of a response variable by a grouping variable
#'
#' In the `data`, one column is a response variable (`response_col`) and another is a predictor or
#' grouping variable (`group_col`). A mean and confidence interval of the response for each value
#' of the predictor is calculated and returned in a data frame. The function most appropriate for
#' factor predictors where grouping is more natural but will also work with other variable types.
#'
#' The response variable must be binary. To compute confidence intervals, values are treated as either
#' 0 or 1. The value given in the argument `pos_class` is treated as 1 (the "positive class").
#' If `pos_class` is not supplied, the positive class will be the "greatest" of the two values
#' using natural ordering (level ordering if target is an ordered factor).
#'
#' @return A summary data frame. By default a plot will also be printed as a side effect.
#' @param dt A data frame.
#' @param response_col Column to use as target variable. Column name (quoted or unquoted) or position.
#' @param group_col Column to use as predictor variable. Column name (quoted or unquoted) or position.
#' @param min_n Integer >= 1. Predictor levels with less than `min_n` observations are not displayed.
#' @param show_all Logical. Defaults to `TRUE`. If `FALSE` will not show levels whose confidence interval
#'   overlaps the mean response of all observations.
#' @param order_n Logical. Whether to force plot and table to order by number of observations of the
#'   predictor. The default setting `NULL` retains ordering if predictor is numeric or ordered factor
#'   and orders by number of observations otherwise.
#' @param conf_level Numeric in (0,1). Confidence level used for confidence intervals.
#' @param pos_class Optional. Specify value in target to associate with class 1.
#' @param plot Logical. If `TRUE`, a plot using `plot_response()` will be printed.
#' @param family Distribution family to use for the response confidence interval. Either "binomial"
#'   or "gaussian".
#'
#' @export
response <- function(dt, response_col, group_col, min_n = 1, show_all = TRUE, order_n = NULL,
                    conf_level = 0.95, pos_class = NULL, plot = TRUE,
                    family = "guess") {
  if (!is.data.frame(dt)){
    stop("`dt` must be a data frame.", call. = FALSE)
  }
  col_names <- names(dplyr::select(dt, {{ response_col }}, {{ group_col }})) # to save as strings
  dt <- dplyr::rename(dt,
                      target = {{ response_col }},
                      var = {{ group_col }})
  if (any(is.na(dt$target))){
    dt <- dplyr::filter(dt, !is.na(.data$target))
    message("There are NA values in target variable. These rows will be excluded from any calculations.")
  }
  if (family == "guess") family <- guess_family(dt$target)

  if (family == "gaussian"){
    if (!(is.numeric(dt$target) | is.logical(dt$target))) {
      stop('Target variable must be numeric or logical with `family = "gaussian"`.', call. = FALSE)
    }
  }

  if (family == "binomial"){
    target_new <- prepare_binomial_response(dt$target, pos_class = pos_class)
    dt <- dplyr::mutate(dt, target = target_new)
    response_classes <- attr(dt$target, "response_classes")
    attr(dt$target, "response_classes") <- NULL
    if (is.null(pos_class)) message(pos_class_message(response_classes))
  }

  if (is.factor(dt$var) && !("(Missing)" %in% dt$var)){
    dt <- dplyr::mutate(dt, var = forcats::fct_explicit_na(.data$var))
  }
  if (is.null(order_n)){
    order_n <- if (is.numeric(dt$var) || is.ordered(dt$var)) FALSE else TRUE
  }

  mean_all <- mean(dt$target)

  res <- dt %>%
    dplyr::group_by(.data$var) %>%
    dplyr::summarise(n = dplyr::n(),
                     n_pos = sum(.data$target),
                     prop = mean(.data$target),
                     mean_response = mean(.data$target),
                     sd = stats::sd(.data$target)) %>%
    dplyr::rename(value = .data$var) %>%
    dplyr::filter(.data$n >= min_n)

  res <- res %>%
    ci_add(conf_level = conf_level, family = family) %>%
    dplyr::mutate(sig = dplyr::case_when(
                    .data$lo > mean_all ~ 3,
                    .data$hi < mean_all ~ 1,
                    T ~ 2),
                  sig = factor(.data$sig, levels = c(1, 2, 3), labels = c("lo", "none", "hi"))
    ) %>%
    purrr::when(!show_all ~ filter(., .data$sig != "none"), ~.) %>%
    purrr::when(order_n ~ dplyr::arrange(., dplyr::desc(.data$n)), ~dplyr::arrange(., .data$value))

  if (family == "binomial"){
    res <- structure(res,
                     response_name = col_names[1],
                     grouping_name = col_names[2],
                     response_classes = response_classes,
                     pos_class_supplied = !is.null(pos_class),
                     mean_all = mean_all)
  }else{
    res <- structure(res,
                     response_name = col_names[1],
                     grouping_name = col_names[2],
                     mean_all = mean_all)
  }

  if (plot){
    print(plot_response(res, order_n = order_n))
  }
  res
}

#' Plot output from `response()`
#'
#' A mean and confidence interval is plotted by group. The axes are flipped so that
#' groups are displayed on the y-axis, with response on the x-axis. A vertical dashed
#' line shows the overall mean of the data. Colours indicate whether the confidence interval
#' crosses the global mean (blue), or is entirely above it (green) or below it (red).
#'
#' @param data Data frame. Output from `response()`.
#' @param order_n Logical. If `TRUE`, groups are ordered by number of observations.
#' @param response_lim Optional x axis limits passed to `ggplot()` e.g. `c(0,1)`.
#' @export
plot_response <- function(data, order_n = FALSE, response_lim  = NULL) {
  mean_all <- attr(data, "mean_all")
  if (order_n){
    data <- dplyr::mutate(data, value = stats::reorder(.data$value, .data$n))
  }
  cols <- c("#F8766D", "#00BA38", "#619CFF")
  gg <- data %>%
    ggplot2::ggplot(aes(x = .data$value, y = .data$prop, color = .data$sig)) +
    ggplot2::geom_point() +
    ggplot2::geom_errorbar(aes(ymin = .data$lo, ymax = .data$hi)) +
    ggplot2::coord_flip() +
    ggplot2::geom_hline(yintercept = mean_all, linetype = 2) +
    ggplot2::ylab("Mean Response") +
    ggplot2::xlab(attr(data, "grouping_name")) +
    ggplot2::theme(legend.position = "none") +
    ggplot2::scale_colour_manual(values = c("lo" = cols[1], "none" = cols[3], "hi" = cols[2])) +
    {if(all(!is.null(response_lim))) ggplot2::ylim(response_lim[1], response_lim[2])}
  gg
}

#' Prepare response vector when family is binomial
#'
#' Converts input to a logical vector with an attribute recording which
#' values in the original vector correspond to positive and negative
#' classes (respectively `TRUE` and `FALSE` in the output).
#'
#' Used in `response()`.
#'
#' @param x Response variable vector.
#' @param pos_class A value in `x` to use as the positive class.
#' @noRd
prepare_binomial_response <- function(x, pos_class = NULL) {
  if (length(stats::na.omit(unique(x))) != 2L){
    stop("Target variable must be binary.", call. = FALSE)
  }
  if (!is.null(pos_class)){
    if (!is.atomic(pos_class) || !length(pos_class) == 1L){
      stop("`pos_class` must be length 1 and atomic.", call. = FALSE)
    }
    if (!pos_class %in% x){
      stop("`pos_class` must match a value in the target vector.", call. = FALSE)
    }
  }
  if (is.null(pos_class)){
    pos_class <- if (is.ordered(x)) levels(x)[2] else sort(unique(as.vector(x)))[2]
  }
  other_class <- setdiff(as.vector(x), pos_class)
  x <- dplyr::if_else(as.vector(x) == pos_class, TRUE, FALSE)
  attr(x, "response_classes") <- c(positive = pos_class, other = other_class)
  x
}

#' Message text when a positive class is not supplied for a binomial response
#'
#' @param classes Length two vector with positive class first.
#' @noRd
pos_class_message <- function(classes) {
  paste0("Argument `pos_class` not supplied. Treating:\n",
  "  * ", glue::double_quote(classes[1]), " as positive class (1).\n",
  "  * ", glue::double_quote(classes[2]), " as other class (0).\n",
  "Use `pos_class` argument to set different value for positive class.")
}

#' Add confidence interval columns to data
#'
#' Called from `response()`. Currently a Gaussian CI is used unless `family = "binomial"`.
#' Input `data` has columns `n`. If family is binomial then also has `prop` column. For
#' Gaussian CI it will have columns `mean_response` and `sd`.
#' @param data Data frame.
#' @param conf_level Numeric scalar in (0, 1). Confidence level for the confidence interval.
#' @param family String.
#' @noRd
ci_add <- function(data, conf_level, family) {
  if (family == "binomial"){
    binom <- binom::binom.wilson(data$prop * data$n, data$n, conf.level = conf_level)
    return(dplyr::mutate(data, lo = binom[["lower"]], hi = binom[["upper"]]))
  }
  dplyr::mutate(data,
                  lo = ci_t(.data$mean_response, .data$sd, .data$n, ci = conf_level, lower = TRUE),
                  hi = ci_t(.data$mean_response, .data$sd, .data$n, ci = conf_level, lower = FALSE))
}

#' t-distribution based confidence interval using summary statistics as input
#'
#' @param mean Mean.
#' @param sd Standard deviation.
#' @param n Number of observations.
#' @param ci Confidence level to use.
#' @param lower Return lower end of CI if `TRUE`, upper otherwise.
#' @noRd
ci_t <- function (mean, sd, n, ci = 0.95, lower = TRUE){
  quan <- ci + (1 - ci)/2
  margin_error <- stats::qt(quan, df = n - 1) * sd / sqrt(n)
  if (lower) return(mean - margin_error)
  mean + margin_error
}

#' Choose response distribution family based on response values
#'
#' Returns `binomial` if `x` has up to two unique, non-missing values.
#' Otherwise return `gaussian` if `x` is numeric and error if neither of these
#' conditions is met.
#' @param x Response vector
#' @noRd
guess_family <- function(x) {
  if (length(stats::na.omit(unique(x))) <= 2L){
    return("binomial")
  }
  if (is.numeric(x)){
    return("gaussian")
  }else{
    stop("Target variable must be binary or numeric.", call. = FALSE)
  }
}

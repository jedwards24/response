#' Confidence intervals for binary target variable by values of a discrete predictor
#'
#' For a data frame input, one variable is a binary target (`target_name`) and another is selected to
#' be a predictor variable (`var_name`). Mean response and a confidence interval is calculated for the
#' target variable for each level or value of the predictor. The results are plotted and returned as a
#' table. The function most appropriate for factor predictors but will work with other variable types also.
#'
#' The target variable must be binary. Top compute confidence intervals this is converted to 0 and 1 values.
#' If it is not obvious which value corresponds to 1 and which to 0 then it will be based on level order
#' if a factor and the first observation otherwise. Giving the value of corresponding to 1 in the argument
#' `pos_class` will override this.
#'
#' Use the `plot` and `return_plot` arguments to control output. By default (designed to be
#'  used interactively) returns a table and prints a plot.  If `return_plot = TRUE` then just the
#'  plot is returned. If  `return_plot = FALSE` and
#'  `plot = FALSE` then the table is returned and no plot is generated. The default
#'
#' @param dt A data frame.
#' @param target_name Column to use as target variable. Column name (quoted or unquoted) or position.
#' @param var_name Column to use as predictor variable. Column name (quoted or unquoted) or position.
#' @param min_n Integer >= 1. Predictor levels with less than `min_n` observations are not displayed.
#' @param show_all Logical. Defaults to `TRUE`. If `FALSE` will not show levels whose confidence interval
#'   overlaps the mean response of all observations.
#' @param order_n Logical. Whether to force plot and table to order by number of observations of the
#'   predictior. The default setting `NULL` retains ordering if predictor is numeric or ordered factor
#'   and orders by number of observations otherwise.
#' @param conf_level Numeric in (0,1). Confidence level used for confidence intervals.
#' @param pos_class Optional. Specify value in target to associate with class 1.
#' @param plot Optional logical. Output a plot or not.
#'
#' @export
response_binom <- function(dt, target_name, var_name, min_n = 1, show_all = TRUE, order_n = NULL,
                    conf_level = 0.95, pos_class = NULL, plot = TRUE) {
  if (!is.data.frame(dt)){
    stop("`dt` must be a data frame.", call. = FALSE)
  }
  y_label <- names(dplyr::select(dt, {{ var_name }})) #for plot
  dt <- dplyr::rename(dt,
                      target = {{ target_name }},
                      var = {{ var_name }})
  if (any(is.na(dt$target))){
    dt <- dplyr::filter(dt, !is.na(.data$target))
    message("There are NA values in target variable. These rows will be excluded from any calculations.")
  }

  targ_vec <- prepare_binomial_response(dt$target, pos_class = pos_class)
  response_classes <- attr(targ_vec, "response_classes")
  if (is.null(pos_class)) pos_class_message(response_classes)
  attributes(targ_vec) <- NULL
  dt <- dplyr::mutate(dt, target = targ_vec)

  if (is.factor(dt$var) && !("(Missing)" %in% dt$var)){
    dt <- dplyr::mutate(dt, var = forcats::fct_explicit_na(.data$var))
  }
  if (is.null(order_n)){
    order_n <- if (is.numeric(dt$var) || is.ordered(dt$var)) FALSE else TRUE
  }

  mean_all <- mean(targ_vec)

  dt_summ <- dt %>%
    dplyr::group_by(.data$var) %>%
    dplyr::summarise(n = dplyr::n(),
                     n_pos = sum(.data$target),
                     prop = mean(.data$target)) %>%
    dplyr::rename(value = .data$var) %>%
    dplyr::arrange(dplyr::desc(.data$n)) %>%
    dplyr::mutate(lo = binom::binom.wilson(.data$prop * .data$n, .data$n, conf.level = conf_level)[['lower']],
                  hi = binom::binom.wilson(.data$prop * .data$n, .data$n, conf.level = conf_level)[['upper']],
                  sig = dplyr::case_when(
                    .data$lo > mean_all ~ 3,
                    .data$hi < mean_all ~ 1,
                    T ~ 2),
                  sig = factor(.data$sig, levels = c(1, 2, 3), labels = c("lo", "none", "hi"))
    ) %>%
    dplyr::filter(.data$n >= min_n) %>%
    purrr::when(!show_all ~ filter(., .data$sig != "none"), ~.) %>%
    purrr::when(order_n ~ dplyr::arrange(., dplyr::desc(.data$n)), ~dplyr::arrange(., .data$value))
  if (plot){
    print(plot_response(dt_summ, order_n = order_n))
  }
  attr(dt_summ, "response_classes") <- response_classes
  attr(dt_summ, "pos_class_supplied") <- !is.null(pos_class)
  dt_summ
}

prepare_binomial_response <- function(x, pos_class = NULL) {
  if (length(na.omit(unique(x))) != 2L){
    stop("Target variable must be binary.", call. = FALSE)
  }
  if (is.null(pos_class)){
    pos_class <- if (is.ordered(x)) levels(x)[2] else sort(unique(as.vector(x)))[2]
  }
  other_class <- setdiff(as.vector(x), pos_class)
  x <- dplyr::if_else(as.vector(x) == pos_class, TRUE, FALSE)
  attr(x, "response_classes") <- c(positive = pos_class, other = other_class)
  x
}

pos_class_message <- function(classes) {
  message("Argument `pos_class` not supplied. Treating:")
  message("  * ", glue::double_quote(classes[1]), " as positive class (1).")
  message("  * ", glue::double_quote(classes[2]), " as other class (0).")
  message("Use `pos_class` argument to set different value for positive class.")
}

#' @param prop_lim Optional x axis limits passed to `ggplot()` e.g. `c(0,1)`.
#'
plot_response <- function(repsonse_dt, order_n = FALSE, prop_lim  = NULL) {
  if (order_n){
    reponse_dt <- dplyr::mutate(response_dt, value = stats::reorder(.data$value, .data$n))
  }
  cols <- c("#F8766D", "#00BA38", "#619CFF")
  gg <- response_dt %>%
    ggplot2::ggplot(aes(x = .data$value, y = .data$prop, color = .data$sig)) +
    ggplot2::geom_point() +
    ggplot2::geom_errorbar(aes(ymin = .data$lo, ymax = .data$hi)) +
    ggplot2::coord_flip() +
    ggplot2::geom_hline(yintercept = mean_all, linetype = 2) +
    ggplot2::ylab("Mean Proportion Target") +
    ggplot2::xlab(y_label) +
    ggplot2::theme(legend.position = "none") +
    ggplot2::scale_colour_manual(values = c("lo" = cols[1], "none" = cols[3], "hi" = cols[2])) +
    {if(all(!is.null(prop_lim))) ggplot2::ylim(prop_lim[1], prop_lim[2])}
  gg
}

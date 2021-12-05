# Rough dev code ----------

# t-distribution based confidence interval using summary statistics as input.
ci_t <- function (mean, sd, n, ci = 0.95, lower = TRUE){
  quan <- ci + (1 - ci)/2
  margin_error <- qt(quan, df = n - 1) * sd / sqrt(n)
  if (lower) return(mean - margin_error)
  mean + margin_error
}

# Continuous response version of prop_ci
norm_ci <- function (dt, target_name, var_name, min_n = 2, show_all = TRUE,
                     order_n = NULL, conf_level = 0.95, y_lim = NULL,
                     plot = TRUE, return_plot = FALSE) {
  if (!is.data.frame(dt)) stop("`dt` must be a data frame.", call. = FALSE)
  y_label <- names(dplyr::select(dt, {{var_name}}))
  dt <- dplyr::rename(dt, target = {{target_name}}, var = {{var_name}})
  if (any(is.na(dt$target))) {
    dt <- dplyr::filter(dt, !is.na(.data$target))
    message("There are NA values in target variable. These rows will be excluded from any calculations.")
  }
  if (!(is.numeric(dt$target) | is.logical(dt$target))) {
    stop("Target variable must be numeric or logical", call. = FALSE)
  }
  if (any(is.na(dt$var)) && is.numeric(dt$var)){
    dt <- dplyr::mutate(dt, var = factor(.data$var, ordered = TRUE))
  }
  if (is.factor(dt$var) && !("(Missing)" %in% dt$var)) {
    dt <- dplyr::mutate(dt, var = forcats::fct_explicit_na(.data$var))
  }
  if (is.null(order_n)) {
    order_n <- if (is.numeric(dt$var) || is.ordered(dt$var)) FALSE else TRUE
  }

  mean_all <- mean(dt$target)
  dt_summ <- dt %>%
    dplyr::group_by(.data$var) %>%
    dplyr::summarise(n = dplyr::n(),
                     mean_response = mean(.data$target),
                     sd = sd(.data$target)) %>%
    dplyr::filter(.data$n >= min_n) %>%
    dplyr::rename(value = .data$var) %>%
    dplyr::arrange(dplyr::desc(.data$n)) %>%
    dplyr::mutate(lo = ci_t(.data$mean_response, .data$sd, .data$n, ci = conf_level, lower = TRUE),
                  hi = ci_t(.data$mean_response, .data$sd, .data$n, ci = conf_level, lower = FALSE),
                  sig = dplyr::case_when(
                    .data$lo >  mean_all ~ 3,
                    .data$hi < mean_all ~ 1,
                    T ~ 2),
                  sig = factor(.data$sig, levels = c(1, 2, 3), labels = c("lo", "none", "hi"))) %>%

    purrr::when(!show_all ~ filter(., .data$sig != "none"), ~.) %>%
    purrr::when(order_n ~ dplyr::arrange(., dplyr::desc(.data$n)), ~dplyr::arrange(., .data$value))
  if (order_n) {
    dt_plot <- dplyr::mutate(dt_summ, value = stats::reorder(.data$value, .data$n))
  }
  else {
    dt_plot <- dt_summ
  }
  if (plot || return_plot) {
    cols <- c("#F8766D", "#00BA38", "#619CFF")
    gg <- dt_plot %>%
      ggplot2::ggplot(aes(x = .data$value, y = .data$mean_response, color = .data$sig)) +
      ggplot2::geom_point() +
      ggplot2::geom_errorbar(aes(ymin = .data$lo, ymax = .data$hi)) +
      ggplot2::coord_flip() +
      ggplot2::geom_hline(yintercept = mean_all, linetype = 2) +
      ggplot2::ylab("Mean Proportion Target") +
      ggplot2::xlab(y_label) + ggplot2::theme(legend.position = "none") +
      ggplot2::scale_colour_manual(values = c(lo = cols[1], none = cols[3], hi = cols[2])) +
      {if (all(!is.null(y_lim))) ggplot2::ylim(y_lim[1], y_lim[2])}
    if (!return_plot)
      print(gg)
  }
  if (return_plot)
    gg
  else dt_summ
}

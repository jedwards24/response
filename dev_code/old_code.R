prepare_binomial_response <- function(targ_vec, pos_class = NULL) {
  if (length(unique(targ_vec)) > 2L){
    stop("Target variable must be binary.", call. = FALSE)
  }
  if (is.factor(targ_vec) && all(targ_vec %in% c("TRUE", "FALSE")))
    targ_vec <- as.numeric(as.logical(targ_vec))
  if (!all(targ_vec %in% c(0L, 1L))){
    if (is.null(pos_class)) pos_class <- as.vector(targ_vec)[1]
    neg_class <- setdiff(as.vector(targ_vec), pos_class)
    targ_vec <- ifelse(targ_vec == pos_class, 1L, 0L)
    message("Target not 0/1 or logical. Treating:")
    message("  * ", glue::double_quote(pos_class), " as positive class (1).")
    message("  * ", glue::double_quote(neg_class), " as negative class (0).")
    message("Use 'pos_class' argument to set different value for positive class.")
  }
  if(is.factor(targ_vec)){
    targ_vec <- as.integer(levels(targ_vec))[targ_vec]
  }
  if (is.logical(targ_vec)) targ_vec <- as.integer(targ_vec)
  targ_vec
}

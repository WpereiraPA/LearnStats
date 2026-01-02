# Funções utilitárias para o pacote LearnStats

#' Validate Input Data
#'
#' Checks if input vectors are valid for statistical tests.
#'
#' @param x First vector
#' @param y Second vector
#' @param paired Whether data should be paired
#' @return Logical indicating if data is valid
#' @keywords internal
.validate_data <- function(x, y, paired = FALSE) {
  if (!is.numeric(x) || !is.numeric(y)) {
    stop("Data must be numeric vectors")
  }

  if (paired && length(x) != length(y)) {
    stop("Paired data must have equal length")
  }

  if (any(is.na(c(x, y)))) {
    warning("NA values detected and will be removed")
  }

  return(TRUE)
}

#' Format p-value for Display
#'
#' Formats p-value according to common statistical conventions.
#'
#' @param p p-value
#' @param digits Number of digits
#' @return Formatted p-value string
#' @export
format_p_value <- function(p, digits = 3) {
  if (p < 0.001) {
    return("< 0.001")
  } else {
    return(paste("=", format.pval(p, digits = digits)))
  }
}

#' Calculate Effect Size (Cohen's d)
#'
#' Calculates Cohen's d for paired or independent samples.
#'
#' @param x First vector
#' @param y Second vector
#' @param paired Whether data is paired
#' @return Cohen's d
#' @export
cohens_d <- function(x, y, paired = FALSE) {
  if (paired) {
    d <- mean(x - y) / stats::sd(x - y)
  } else {
    n1 <- length(x)
    n2 <- length(y)
    pooled_sd <- sqrt(((n1 - 1) * stats::var(x) + (n2 - 1) * stats::var(y)) / (n1 + n2 - 2))
    d <- (mean(x) - mean(y)) / pooled_sd
  }
  return(d)
}






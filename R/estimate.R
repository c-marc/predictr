#' Get likelihood ratios
#'
#' @param FPF False Positive Fraction
#' @param TPF True Positive Fraction
#'
#' @return a list with positive and negative likelihood ratios
getLRatios <- function(FPF, TPF){
  list(
    negative = (1-TPF) / (1-FPF),
    positive = TPF / FPF
  )
}

#' Get the odds ratio from (FPF, TPF)
#'
#' @param FPF False Positive Fraction
#' @param TPF True Positive Fraction
#'
#' @return a value
getOR <- function(FPF, TPF){
  logOR <- qlogis(TPF) - qlogis(FPF)
  exp(logOR)
}

#' Get couples of (FPF, TPF) with the same OR
#'
#' @param FPF False Positive Fraction
#' @param TPF True Positive Fraction
#'
#' @return a tibble with of FPF and TPF values
getIsoOR <- function(FPF, TPF) {
  OR <- getOR(FPF, TPF)
  x <- seq(0, 1, length.out = 101)
  tibble(FPF = x) %>%
    mutate(TPF = ifelse(FPF %in% c(0, 1),
      FPF,
      plogis(qlogis(x) + log(OR))
    ))
}

#' Get couples of (FPF, TPF) with the same LR
#'
#' @param FPF False Positive Fraction
#' @param TPF True Positive Fraction
#'
#' @return a tibble with intercept and slope for each LR
getIsoLR <- function(FPF, TPF){
  lRatios <- getLRatios(FPF, TPF)
  tribble(
    ~type, ~intercept, ~slope,
    "positive", 0, lRatios$positive,
    "negative", 1 - lRatios$negative, lRatios$negative
  )
}


getPredValues <- function(FPF, TPF, prior = NULL) {
  # get LR as a list and transform in a tidy format
  lRatios <- getLRatios(FPF, TPF) %>%
    as_tibble() %>%
    pivot_longer(c(positive, negative), names_to = "type")

  # if prior unspecified, compute over ]0-1[
  if (is.null(prior)) prior <- seq(0, 1, length.out = 101)

  dat <- expand_grid(
    prior = prior,
    type = c("positive", "negative")
  )

  inner_join(dat, lRatios, by = "type") %>%
    mutate(posterior = ifelse(prior %in% c(0, 1),
      prior,
      plogis(qlogis(prior) + log(value))
    ))
}


prettyProp <- function(prop, digits = 0){
  paste0(round(100*prop, digits), "%")
}

prettyRatio <- function(ratio, digits = 2){
  if(ratio >= 1){
    paste0(signif(ratio, digits), " : 1")
  }else{
    paste0("1 : ", signif(1/ratio, digits))
  }
}

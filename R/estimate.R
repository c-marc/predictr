#' Get likelihood ratios
#'
#' @param FPF False Positive Fraction
#' @param TPF True Positive Fraction
#'
#' @return a list with positive and negative likelihood ratios
#' @export
#' 
#' @examples
#' getLRatios(.4, .6)
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
#' @export
#' @importFrom stats qlogis plogis
#' 
#' @examples
#' getOR(.4, .6)
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
#' @export
#' @importFrom stats qlogis plogis
getIsoOR <- function(FPF, TPF) {
  OR <- getOR(FPF, TPF)
  x <- seq(0, 1, length.out = 101)
  tibble::tibble(FPF = x) %>%
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
#' @export
getIsoLR <- function(FPF, TPF){
  lRatios <- getLRatios(FPF, TPF)
  tibble::tribble(
    ~type, ~intercept, ~slope,
    "positive", 0, lRatios$positive,
    "negative", 1 - lRatios$negative, lRatios$negative
  )
}


#' Get predictive values
#'
#' @param FPF False Positive Fraction
#' @param TPF True Positive Fraction
#' @param prior a vector of prior probabilities. If NULL, will compute over (0,1)
#'
#' @return a tibble with:
#' - prior
#' - type: "negative" or "positive" 
#' - posterior: posterior probabilities
#' @export
#' @importFrom dplyr inner_join mutate
#' @examples
#' getPredValues(.4, .6, .5)
getPredValues <- function(FPF, TPF, prior = NULL) {
  # get LR as a list and transform in a tidy format
  lRatios <- getLRatios(FPF, TPF) %>%
    tibble::as_tibble() %>%
    tidyr::pivot_longer(c(positive, negative), names_to = "type")

  # if prior unspecified, compute over ]0-1[
  if (is.null(prior)) prior <- seq(0, 1, length.out = 101)

  dat <- tidyr::expand_grid(
    prior = prior,
    type = c("positive", "negative")
  )

  inner_join(dat, lRatios, by = "type") %>%
    mutate(posterior = ifelse(prior %in% c(0, 1),
      prior,
      plogis(qlogis(prior) + log(value))
    ))
}


#' Prettify proportions
#'
#' @param prop A proportion
#' @param digits passed to round()
#'
#' @return A pretty string
#' @export
#'
#' @examples
#' prettyProp(.501)
prettyProp <- function(prop, digits = 0){
  paste0(round(100*prop, digits), "%")
}

#' Prettify ratios
#'
#' @param ratio A ratio
#' @param digits passed to signif() 
#'
#' @return A pretty string
#' @export
#'
#' @examples
#' prettyRatio(1/2.41)
prettyRatio <- function(ratio, digits = 2){
  if(ratio >= 1){
    paste0(signif(ratio, digits), " : 1")
  }else{
    paste0("1 : ", signif(1/ratio, digits))
  }
}

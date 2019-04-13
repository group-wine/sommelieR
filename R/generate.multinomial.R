#' A function to create an n x j matrix of outcomes where the rows are subjects and columns are the ordinal levels of an ordinal outcome variable.
#'
#' This function takes a numeric vector corresponding to observed ordinal values of a random variable, and a second vector specifying the ordered levels of y. This is not intended to be used independently.
#'
#' @param y A vector of containing the values of an ordinal outcome variable.
#' @param y.levels A vector of the unique, ordinal levels of y.
#'
#' @return A matrix where the rows correspond to subjects and the columns correspond to levels of the outcome.
#'
#' @useDynLib sommelieR
#' @export
generate.multinomial <- function(y, y.levels){

  ######################################
  #Arguments:
  #
  #   y: vector that contains the observed, ordinal outcomes for each individual
  #   y.levels: a vector that specifies the ordinal order of the categories of y
  #
  ##############################################

  #initiate matrix where rows are sujects, columns are ordinal values
  y.multinomial <- matrix(rep(NA, length(y)*length(y.levels)), nrow = length(y))

  #fill in matrix
  for (i in 1:length(y)){

    subject.val <- y[i]
    y.multinomial[i, ] <- 1*(y.levels == subject.val)
  }

  #return matrix
  colnames(y.multinomial) <- as.character(y.levels)
  return(y.multinomial)

}

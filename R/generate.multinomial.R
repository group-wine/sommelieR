#function to create an n x j matrix of outcomes where the rows are subjects and columns
#are the ordinal levels of an ordinal outcome variable
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

#' Obtain total sum of squares
#'
#' TSS takes in two parameters, a dataframe and a column nd returns the Total Sum of Squares
#'
#' @param df Data Frame
#'
#' @param column column name
#'
#' @return Variable
#'
#' @examples
#' col1 <- c(5, 6, 7, 8)
#' col2 <- c(1, 8, 3, 4)
#' df.ex <- data.frame(col1, col2)
#' TSS(df.ex, col2)
#'
#' @export
TSS <- function(df, column){
  column <- eval(substitute(column), df, parent.frame())
  tss <- with(df, sum((column - mean(column))^2))
  return(tss)
}

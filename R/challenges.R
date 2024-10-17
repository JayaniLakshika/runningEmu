#' @title First function
#' @description
#' Given a square matrix, calculates the average over the sum of row averages and column averages
#'
#' @param m a square matrix with no missing values
#' @return Single numerical value
#' @examples
#' m <- matrix(seq(16),nrow=4)
#' un(m)
#' @export
un <- function(m) {
  row_means <- rowMeans(m)  # Calculate row means once
  col_means <- colMeans(m)  # Calculate column means once
  mean(row_means + col_means)
}

#' @title Second
#' @description
#' Given a vector gives the longest continuous increasing subset
#'
#' @param vec Numerical vector with no missing values
#' @return A numerical vector containing the longest continuous increasing subset
#' @export
deux <- function(vec) {
  reduce(2:length(vec), .init = list(longest = vec[1], current = vec[1]),
         .f = function(state, i) {
           if (vec[i] > vec[i - 1]) {
             # Continue current increasing subsequence
             state$current <- c(state$current, vec[i])
           } else {
             # Compare and reset current subsequence
             if (length(state$current) > length(state$longest)) {
               state$longest <- state$current
             }
             state$current <- vec[i]
           }
           state
         }) %>%
    {
      # Final check for the last subsequence
      if (length(.$current) > length(.$longest)) {
        return(.$current)
      } else {
        return(.$longest)
      }
    }
}

#' @title Third
#' @description
#' Given a vector return the count of each unique element
#'
#' Hint: Try looking into `tabulate`, `fastmatch::fastmatch`
#' @param vec Numerical vector
#' @return A single numerical vector with counts of each unique element
#'
#' @export
trois <- function(vec) {
  tabulate(vec) |>
    rbind(unique(vec))

}

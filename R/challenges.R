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
  n <- length(vec)
  if (n == 0) return(c())  # Handle empty vector case

  longest_seq_start <- 1
  longest_seq_length <- 1
  current_seq_start <- 1
  current_seq_length <- 1

  for (i in 2:n) {
    if (vec[i] > vec[i - 1]) {
      # Continue current increasing subsequence
      current_seq_length <- current_seq_length + 1
    } else {
      # Compare and reset current subsequence
      if (current_seq_length > longest_seq_length) {
        longest_seq_start <- current_seq_start
        longest_seq_length <- current_seq_length
      }
      current_seq_start <- i  # Start a new subsequence
      current_seq_length <- 1
    }
  }

  # Final check after the loop
  if (current_seq_length > longest_seq_length) {
    longest_seq_start <- current_seq_start
    longest_seq_length <- current_seq_length
  }

  return(vec[longest_seq_start:(longest_seq_start + longest_seq_length - 1)])
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
  table(vec)
}

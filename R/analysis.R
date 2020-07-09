#' Take bootstrap samples from a vector
#'
#' Creates a matrix containing bootstrap samples from your original data
#'
#' @param vec The vector you want to sample from (often a column from a dataframe)
#' @param num_samples The number of bootstrap samples you want to take
#'
#' @return A matrix of num_samples columns each containing a resampled dataset
#'
#' @examples
#' bootstrap_sample(mtcars$mpg, 15000)
bootstrap_sample <- function(vec, num_samples) {
    len <- length(vec)
    samples <- replicate(num_samples, sample(variable, len, replace=TRUE))
    return(samples)
}

#' Calculate means from bootstrap samples
#'
#' Calculates a vector of means from a matrix of boostrap samples
#'
#' @param mat A matrix where each column is one bootstrapped sample
#'
#' @return A vector containing the mean for each boostrapped sample
#'
#' @examples
#' bootstrap_means(mat)
bootstrap_means <- function(mat) {
    means <- apply(mat, 2, mean)
    return(means)
}

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
    samples <- replicate(num_samples, sample(vec, len, replace=TRUE))
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

#' Generates normal quantiles from a sample set
#'
#' Generates a set of normal quantiles to compare to your data to check for normality
#'
#' @param vec A vector containing your actual data
#'
#' @return A vector containing the sample normal quantiles
generate_quantiles_normal <- function(vec) {
    nv <- sort(vec)
    i <- 1:length(vec)
    fi <- (i-0.5)/length(vec)
    xnorm <- qnorm(fi)
    return(xnorm)
}

#' Generate a qq plot checking for a normal distribution
#'
#' Generates a plot of sample quantiles to your actual data to check for normality.
#' It includes a 95% confidence interval.
#'
#' @param vec A vector containing your actual data
#' @param qs A vector containing sample quantiles
#'
#' @return A ggplot object
qq_plot_normal <- function(vec, qs) {
    require(ggplot2)
    nv <- sort(vec)
    fit <- lm(nv~qs)
    p <- ggplot(mapping=aes(x=qs, y=nv)) +
        geom_point() +
        geom_smooth(method="lm", color="red", se=TRUE)
    print(p)
    return(p)
}

#' Build a comparison & plot to see visual normality check
#'
#' Constructs sample quantiles & compares them to your data to enable a quick visual check for normality
#'
#' @param vec A vector containing your actual data
#'
#' @return A ggplot object
visual_normality_check <- function(vec) {
    require(ggplot2)
    p1 <- ggplot() +
        geom_density(mapping=aes(x=vec), fill="blue", alpha="0.5")
    print(p1)
    qs <- generate_quantiles_normal(vec)
    p <- qq_plot_normal(vec, qs)
    return(p)
}

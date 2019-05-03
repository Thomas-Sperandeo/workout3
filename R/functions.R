#private checker function to check if our input prob is valid
prob <- 0
check_prob <- function(prob){
  if(length(prob) != 1){
    stop("probability should only have length one")
  } else if(typeof(prob) != "double"){
    stop("probability must be a real number between 0 and 1")
  } else if(prob <= 0 | prob >= 1){
    stop("probability must be a real number between 0 and 1")
  } else {
    TRUE
  }
}

#private checker function to check if our input trials is valid
trials <- 0
check_trials <- function(trials){
  if(trials %% 1 == 0 & trials > 0 & length(trials) == 1){
    TRUE
  } else {
    stop("trials must be a positive integer")
  }
}

#private checker function to check if our input success is a valid number of successes
success <- c()
check_success <- function(trials, success){
  for(i in 1:length(success)){
    if(success[i] %% 1 != 0){
      stop("success must be a vector of positive integers")
    } else if(success[i] < 0){
      stop("success must be a vector of positive integers")
    } else if(max(success) > trials){
      stop("success must be smaller than trials")
    } else {
      return(TRUE)
    }
  }
}

#private auxiliary function to compute mean
aux_mean <- function(trials, prob){
  mean <- trials * prob
  return(mean)
}

#private auxiliary function to compute variance
aux_variance <- function(trials, prob){
  variance <- (trials * prob) * (1- prob)
  return(variance)
}

#private auxiliary function to compute mode
aux_mode <- function(trials, prob){
  if(((trials * prob) + prob) %% 1 == 0 & ((trials * prob) + prob) > 0){
    mode <- c((trials * prob) + prob, (trials * prob) + prob - 1)
    return(mode)
  } else {
    mode <- ((trials * prob) + prob) - ((trials * prob) + prob) %% 1
    return(mode)
  }
}


#private function to compute skewness
aux_skewness <- function(trials, prob){
  skewness <- (1 - (2 * prob)) / ((trials * prob) * (1 - prob))^0.5
  return(skewness)
}

#private auxiliary function to compute kurtosis
aux_kurtosis <- function(trials, prob){
  kurtosis <- (1 - (6 * prob) * (1 - prob)) / ((trials * prob) * (1 - prob))
  return(kurtosis)
}

#' @title bin_choose
#' @description function which computes number of permutations for k successes in n trials
#' @param trials a non-negative integer which provides the number of trials
#' @param success a non-negative integer, smaller or equal to n, which provides the number of successes in n trials
#' @return an object of numeric class, number of permutations for given success and trials
#' @export
#' @examples
#' #default
#' 1_choose_1 <- bin_choose(1, 1)
#'
#' #second example
#' 10_choose_4 <- bin_choose(10, 4)
#'
#' #0 successes
#' 5_choose_0 <- bin_choose(5, 0)
choose <- 0
bin_choose <- function(trials = 1 , success = 1){
  if(max(success) > trials){
    stop("number of successes cannot exceed number of trials")
  }
  choose <- factorial(trials)/ ((factorial(success)) * factorial(trials - success))
  return(choose)
}

#' @title bin_probability
#' @description function which computes binomial probability for k successes in n trials
#' @param trials a non-negative integer which provides the number of trials
#' @param success a non-negative integer, smaller or equal to n, which provides the number of successes in n trials
#' @param prob a number between 0 and 1 which gives the probablity of a success on an individual draw
#' @return an object of numeric class, binomial probability for given successes and trials
#' @export
#' @examples
#'
#' #default
#' prob1 <- bin_probability(1, 1, 0.5)
#'
#' #multiple number of successes
#' prob2 <- bin_probablity(10, 1:5, 0.3)
#'
bin_probability <- function(trials, success, prob){
  probability1 <- bin_choose(trials, success) * (prob)^success * (1 - prob)^(trials - success)
  return(probability1)
}

bin_probability(1, 0, 0.5)

#' @title bin_distribution
#' @description function which computes probabilities for getting exactly k successes in n trials with probability p
#' @param trials a non-negative integer which provides the number of trials
#' @param prob a number between 0 and 1 which gives the probablity of a success on an individual draw
#' @return an object of bindis class, binomial distribution of probabilities
#' @export
#' @examples
#'
#' default
#' dis1 <- bin_distribution(1, 0.5)
#'
#' #example 2
#' dis2 <- bin_distribution(10, 0.2)
#'
#'
probability <- 0
bin_distribution <- function(trials, prob){
  successes <- 0:trials
  for(i in 1:(trials + 1)){
    probability[i] <- bin_probability(trials, i - 1, prob)
  }
  bindis <- data.frame(successes, probability)
  class(bindis) <- c("bindis", "data.frame")
  return(bindis)
}

#' @export
plot.bindis <- function(trials, prob){
  plot <- ggplot(bin_distribution(trials, prob), aes(x = successes, y = probability)) +
    geom_histogram(stat = "identity", binwidth = 1, center = 0.5)
  return(plot)
}

plot.bindis(10, 0.5)

#' @title bin_cumulative
#' @description function which computes culmulative probablity for binomial distributions for given inputs trials and prob
#' @param trials a non-negative integer which provides the number of trials
#' @param prob a number between 0 and 1 which gives the probablity of a success on an individual draw
#' @return an object of bincum class, cumulative probability of binomial distribution
#' @export
#' @examples
#'
#' #default
#' bin_cumulative(1, 0.5)
#'
#' #example 2
#' bin_cumulative(10, 0.8)
#'
bin_cumulative <- function(trials = 1, prob = 0.5){
  successes <- 0:trials
  for(i in 1:(trials +1)){
    probability[i] <- bin_probability(trials, i - 1, prob)
    cumulative <- cumsum(probability)
  }
  bin_cum <- data.frame(successes, probability, cumulative)
  class(bin_cum) <- c("bincum", "data.frame")
  return(bin_cum)
}

bin_cumulative(5, 0.5)

#' @export
plot.bincum <- function(trials, prob){
  plot_cum <- ggplot(bin_cumulative(trials, prob), aes(x = successes, y = cumulative)) +
    geom_line() + geom_point() + theme_classic() + ylab("cumulative probability")
  return(plot_cum)
}

plot.bincum(5, 0.5)

#' @title bin_variable
#' @description function which checks if our variable is binomial and dispays parameters
#' @param trials a non-negative integer which provides the number of trials
#' @param prob a number between 0 and 1 which gives the probablity of a success on an individual draw
#' @return an object of binvar class, number of trials and the probability of an individual success, an object of class \code{"binvar"}
#' @export
#' @examples
#'
#' #default
#' prob1 <- bin_variable(1, 0.5)
#'
#' #example 2
#' prob2 <- bin_variable(10, 0.3)
#'
list <- list()
prob <- 0
bin_variable <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)
  list <- list(trials = trials, prob = prob)
  class(list) <- "binvar"
  return(list)
}

#' @export
print.binvar <- function(list = "binvar"){
  cat('"Binomial variable"', "\n\n")
  cat("Parameters", "\n")
  cat("- number of trials:", list$trials, "\n")
  cat("- probability of success:", list$prob, "\n")
}

#' @export
x <- list()
summary.binvar <- function(list1 = "binvar") {
  x <-
    list(
      trials = list1$trials,
      prob = list1$prob,
      mean = aux_mean(list1$trials, list1$prob),
      mode = aux_mode(list1$trials, list1$prob),
      kurtosis = aux_kurtosis(list1$trials, list1$prob),
      skewness = aux_skewness(list1$trials, list1$prob)
    )
  class(x) <- "summary.binvar"
  return(x)
}

#' @export
print.summary.binvar <- function(list1 = "summary.binvar"){
  cat('"Summary Binomial"', "\n\n")
  cat("Parameters", "\n")
  cat("- number of trials:", list1$trials, "\n")
  cat("- probability of success:", list1$prob, "\n\n")
  cat("Measures", "\n")
  cat("- mean:", aux_mean(list1$trials, list1$prob), "\n")
  cat("- variance:", aux_variance(list1$trials, list1$prob), "\n")
  cat("- mode:", aux_mode(list1$trials, list1$prob), "\n")
  cat("- skewness:", aux_skewness(list1$trials, list1$prob), "\n")
  cat("- kurtosis:", aux_kurtosis(list1$trials, list1$prob), "\n")
}

#' @title bin_mean
#' @description function which computes mean of binomial distribution with given inputs trials and prob
#' @param trials a non-negative integer which provides the number of trials
#' @param prob a number between 0 and 1 which gives the probablity of a success on an individual draw
#' @return an object of numeric class, mean of binomial distribution
#' @export
#' @examples
#'
#' #default
#' binmean1 <- bin_mean(10, 0.3)
#'
#' #example2
#' binmean2 <- bin_mean(100, 0.2)
#'
bin_mean <- function(trials, prob){
  if(check_trials(trials) == TRUE & check_prob(prob) == TRUE){
    return(aux_mean(trials, prob))
  } else {
    stop("invalid inputs")
  }
}

#' @title bin_variance
#' @description function which computes variance of binomial distribution with given inputs trials and prob
#' @param trials a non-negative integer which provides the number of trials
#' @param prob a number between 0 and 1 which gives the probablity of a success on an individual draw
#' @return an object of numeric class, variance of binomial distribution
#' @export
#' @examples
#'
#' #default
#' binvar1 <- bin_variance(10, 0.3)
#'
#' #example2
#' binvar2 <- bin_variance(100, 0.2)
#'
bin_variance <- function(trials, prob){
  if(check_trials(trials) == TRUE & check_prob(prob) == TRUE){
    return(aux_variance(trials, prob))
  } else {
    stop("invalid inputs")
  }
}

#' @title bin_mode
#' @description function which computes mode of binomial distribution with given inputs trials and prob
#' @param trials a non-negative integer which provides the number of trials
#' @param prob a number between 0 and 1 which gives the probablity of a success on an individual draw
#' @return an object of numeric class, mode of binomial distribution
#' @export
#' @examples
#'
#' #default
#' binmode1 <- bin_mean(10, 0.3)
#'
#' #example2
#' binmode2 <- bin_mean(5, 0.5)
#'
bin_mode <- function(trials, prob){
  if(check_trials(trials) == TRUE & check_prob(prob) == TRUE){
    return(aux_mode(trials, prob))
  } else {
    stop("invalid inputs")
  }
}

#' @title bin_kurtosis
#' @description function which computes kurtosis of binomial distribution with given inputs trials and prob
#' @param trials a non-negative integer which provides the number of trials
#' @param prob a number between 0 and 1 which gives the probablity of a success on an individual draw
#' @return an object of numeric class, kurtosis of binomial distribution
#' @export
#' @examples
#'
#' #default
#' binkurtosis1 <- bin_kurtosis(10, 0.3)
#'
#' #example2
#' binkurtosis2 <- bin_kurtosis(100, 0.2)
#'
bin_kurtosis <- function(trials, prob){
  if(check_trials(trials) == TRUE & check_prob(prob) == TRUE){
    return(aux_kurtosis(trials, prob))
  } else {
    stop("invalid inputs")
  }
}

#' @title bin_skewness
#' @description function which computes skewness of binomial distribution with given inputs trials and prob
#' @param trials a non-negative integer which provides the number of trials
#' @param prob a number between 0 and 1 which gives the probablity of a success on an individual draw
#' @return an object of numeric class, skewness of binomial distribution
#' @export
#' @examples
#'
#' #default
#' binskewness1 <- bin_skewness(10, 0.3)
#'
#' #example2
#' binskweness2 <- bin_skewness(100, 0.2)
#'
bin_skewness <- function(trials, prob){
  if(check_trials(trials) == TRUE & check_prob(prob) == TRUE){
    return(aux_skewness(trials, prob))
  } else {
    stop("invalid inputs")
  }
}

library(rstan)


fit_model <- function(dat, stanfile = "beta_simple.stan") {
  
  dat$x <- ifelse(dat$pre_crastinate == "yes", 0.5, -0.5)
  X <- cbind(1, dat$x)
  
  stan_df <- list(
    N = nrow(dat),
    K = ncol(X),
    y = dat$search_strat,
    X = X)
  
  m <- stan(
    file = stanfile,
    data = stan_df,
    chains = 4,
    warmup = 2000,
    iter = 5000,
    refresh = 100)
  
  return(m)
  
}

gen_plot_pred <- function(dat, m, only_plot_prior = FALSE) {
  
  samples <- rstan::extract(m)

  pred_dat <- tibble(pre_crastinate = c(-0.5, 0.5))

  X = as.matrix(pred_dat)
  X <- cbind(1, X)

  # Look at prior predictions for mu
  mu <- array(0, dim = c(100000, nrow(X)))
  for (ii in 1:100000) {
    # generate a random beta from priors
    beta <- as.vector(c(
      rnorm(1, mean=0, sd=.5),
      rnorm(1, mean=0, sd=.1)
    ))
    mu[ii, ] <- plogis(X %*% beta)
  }

  mu_prior <- tibble(pre_crastinate = rep(c("no", "yes", "difference"), each = 100000),
                     mu  = c(mu[,1], mu[,2], mu[,2] - mu[,1])) %>%
    mutate(pre_crastinate = if_else(pre_crastinate == "difference", 
                                    "prior for difference", 
                                    "prior"))
  
 
  # Look at posterior predictions for mu
  if (only_plot_prior == FALSE) {
  mu <- array(0, dim = c(nrow(samples$beta), nrow(X)))
  for (ii in 1:nrow(samples$beta)) {
    mu[ii, ] <- plogis(X %*% samples$beta[ii, ])
  }
  

  mu_df <- tibble(pre_crastinate = rep(c("no", "yes", "difference"), each = 12000),
                  mu  = c(mu[,1], mu[,2], mu[,2] - mu[,1])) %>%
    bind_rows(mu_prior) %>%
    mutate(pre_crastinate = as_factor(pre_crastinate))
  
  my_cols <-  c(viridis::viridis(3)[1], viridis::viridis(3)[3], viridis::viridis(3)[2], "#444444", "#999999")
  my_edges = c("black", "black", "black", "#444444", "#999999")
  
  mu_df <- mu_df %>% filter(!(pre_crastinate %in% c("prior", "prior for difference")))
  } 
  else
  {
    mu_df <- mu_prior
    my_cols <- c("#444444", "#999999")
    my_edges = c(NA, NA)
  }
  
  
  ggplot(mu_df, aes(x = mu, fill = pre_crastinate, colour = pre_crastinate)) +
    geom_density(alpha = 0.5) +
    geom_vline(xintercept = 0, linetype = 2) +
    scale_x_continuous(
      "group mean proportion estimates", limits = c(-0.1, 0.82), expand = c(0,0)) +
    scale_fill_manual("pre-crastinate", values = my_cols) +
    scale_colour_manual("pre-crastinate", values = my_edges) +
    guides(colour = guide_legend(show = FALSE), alpha = guide_legend(show = FALSE)) +
    theme_bw() -> plt
  
  return(plt)
  
}






make_predictions <- function(m, pred_dat, x) {
  post <- rstan::extract(m)
  
  beta <- colMeans(post$beta)
  gamma <- colMeans(post$gamma)
  
  mu  <- plogis(X %*% beta)
  phi <- exp(X %*% gamma)
  
  A <- mu * phi 
  B <- (1 - mu) * phi
  
  n <- length(x)
  
  p <- unlist(map2(A, B, dbeta, x = x))
  
  return(p)
}

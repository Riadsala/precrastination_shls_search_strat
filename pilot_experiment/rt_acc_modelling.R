library(MASS)
library(extraDistr)
library(tidyverse)
library(fitdistrplus)


sim_race_model <- function(mu, lambda, n = 1000) {
	# mu and lambda are lists with TA and TP entries
	# function will simulate n TA and n TP trials

	# need to define mu for Present race for TA trials
	mus <- c(
		race_a_stim_a_mu = mu$ta,
		race_p_stim_a_mu = 100,
		race_a_stim_p_mu = mu$ta,
		race_p_stim_p_mu = mu$tp)

	lambdas <- c(
		race_a_stim_a_lambda = lambda$ta,
		race_p_stim_a_lambda = lambda$tp,
		race_a_stim_p_lambda = lambda$ta,
		race_p_stim_p_lambda = lambda$tp)

	args <- list(mu = mus, lambda = lambdas)
	races <- purrr::pmap(args, ~rwald(n, ..1, ..2))

	sim_dat <- races_to_rt_acc(races)

	return(sim_dat)
}

races_to_rt_acc <- function(races) {
  # this function coverts race simulations in to rt and accuracy

	n <- length(races[[1]])
	rt_ta <- pmin(races$race_p_stim_a_mu, races$race_a_stim_a_mu)
	rt_tp <- pmin(races$race_p_stim_p_mu, races$race_a_stim_p_mu)

	acc_ta <- ifelse(races$race_a_stim_a_mu <= races$race_p_stim_a_mu, 1, 0)
	acc_tp <- ifelse(races$race_p_stim_p_mu <= races$race_a_stim_a_mu, 1, 0)

	sim_data <- tibble(
		target = as_factor(rep(c("absent", "present"), each = n)),
		rt  = c(rt_ta, rt_tp),
		acc = c(acc_ta, acc_tp))

	return(sim_data)
}

split_data <- function(d) {
  # split data into list of hits, false pos., false neg., and misses
  
  out <- list(
    true_pos  = filter(d, targSide == "hard", accuracy = 1),
    false_pos = filter(d, targSide == "absent", accuracy = 0),
    false_neg = filter(d, targSide == "hard", accuracy = 0),
    true_neg  = filter(d, targSide == "absent", accuracy = 1)
  )
  
  return(out)
  
}
		
loglik_of_dat_givn_params <- function(d, params) {
  
  # first, absent responses on absent trials
  mu <- params$ta$mu
  lambda <- params$ta$lambda
  
  d %>% filter(targSide == "absent", accuracy == 1) -> d
  
  loglik <- sum(dwald(x = d$RT, mu=mu, lambda=lambda, log = TRUE))
  
  return(loglik) 
}




# import data
d <- as_tibble(readRDS("scratch/processedRTandAccData.Rda"))

# fit the absent-race to the TA responses
# assume I can do this indep. from TP responses

d %>% 
	dplyr::select(-condition) %>% 
	filter(
	  ((targSide == "absent")  & (accuracy == 1)) |
	  ((targSide == "present") & (accuracy == 0))) %>%
	mutate(
		targSide = fct_drop(as_factor(targSide))) -> d_ta


wald_fit_ta =  fitdist(d_ta$RT, 'wald', start=list(mu=1,lambda=1))$estimate

params <- tibble(ta = list(mu = wald_fit_ta[[1]], lambda = wald_fit_ta[[2]]))

loglik_of_dat_givn_params(d, params)

# plot empirical TA and wald fit
p_rt <- dwald(
  x = seq(0, 80, 0.1), 
  lambda = wald_fit_ta$estimate[2], 
  mu = wald_fit_ta$estimate[1])

ggplot() +
  geom_density(data = d_ta, aes(x = RT), fill = 'lightblue') +
  geom_path(aes(x = seq(0, 80, 0.1), y = p_rt)) +
  coord_cartesian(xlim = c(0, 80))





d_ta %>%
	group_by(subject, targSide) %>%
	summarise(
		lambda = fitdist(RT, 'wald', start=list(mu=1,lambda=1))$estimate[2],
		mu = fitdist(RT, 'wald', start=list(mu=1,lambda=1))$estimate[1],
		n = n()) %>%
	mutate(
		vu = 1/mu,
		sigma = sqrt(1/lambda))-> d2 

p <- purrr::pmap(list(lambda=d2$lambda, mu =d2$mu), ~dwald(seq(0, 80, 0.1), ..2, ..1))

dp <-tibble(
	subject = rep(d2$subject, each = 801), 
	targSide = rep(d2$targSide, each = 801), 	
	RT = rep(seq(0, 80, 0.1), nrow(d2)),
	y = unlist(p))

plt <- ggplot()
plt <- plt + geom_density(data = filter(d, subject == 1), aes(x = RT,fill = targSide), alpha = 0.75, colour = NA)
plt <- plt + geom_path(data = filter(dp, subject ==1), aes(x = RT, y = y), size = 0.5, colour = "blue", alpha = 0.75)

plt <- plt + facet_wrap(~ subject, scales = "free_y")
# plt <- plt + scale_x_log10()
ggsave('test.pdf')


d_pre_cras <- read_csv("level3_summary.csv")
d_pre_cras$pre_crastinate <- as_factor(ifelse(d_pre_cras$pre_crastinate == 0, "no", "yes"))

d_pre_cras %>% select(subject = participant, pre_crastinate ) %>%
	mutate(subject = as.factor(subject)) %>%
	left_join(d2, by = "subject") %>%
	filter(!is.na(pre_crastinate), n > 20) %>%
	select(subject, pre_crastinate, n, vu, sigma) %>%
	gather(vu, sigma, key = "param", value = value) -> d


plt <- ggplot(d, aes(fill = pre_crastinate, x = value))
plt <- plt + geom_density(alpha = 0.3)
plt <- plt + scale_fill_viridis_d()
plt <- plt + facet_wrap(~ param)
ggsave("alasdair_gets_ahead_of_himself.pdf")

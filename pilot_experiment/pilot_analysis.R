library(rethinking)
library(tidyverse)
library(rstan)
options(digits=5)

d <- read_csv("level3_summary.csv")

d$pre_crastinate <- as_factor(ifelse(d$pre_crastinate == 0, "no", "yes"))

ggplot(d, aes(x = pre_crastinate, fill = pre_crastinate)) + 
	geom_bar(colour = "black", alpha = 0.5) + 
	scale_fill_viridis_d() + scale_x_discrete("pre-crastinate") +
	theme_bw() + theme(legend.position = "none") 
ggsave("pre-crastinate_counts.png", width  = 4, height = 4)
############################
# Look at RT
############################


(d %>% 
	select(participant, pre_crastinate,easy = rt_easy, hard = rt_hard, absent = rt_absent) %>%

	gather(easy, hard, absent, key = "trial_type", value = "rt") %>%
	mutate(trial_type = as_factor(trial_type))) -> d2


plt <- ggplot(d2, aes(x = trial_type, y = log2(rt), fill = pre_crastinate)) 
plt <- plt + geom_boxplot(alpha = 0.5)
plt <- plt + scale_fill_viridis_d("pre-crastinate?")
plt <- plt + scale_y_continuous("reaction time (seconds)", breaks = 0:5, labels = 2^(0:5) )
plt <- plt + scale_x_discrete("trial type")
plt <- plt + theme_bw()
plt <- plt + theme(
	legend.justification=c(1,0), 
	legend.position=c(1,0),
	legend.background = element_rect(size = 0.25, color = "black"))


ggsave("scratch/rt.pdf", width = 4, height = 3)
ggsave("scratch/rt.png", width = 4, height = 3)



############################
# Look at ACC
############################


(d %>% 
	select(participant, pre_crastinate,easy = acc_easy, hard = acc_hard, absent = acc_absent) %>%
	gather(easy, hard, absent, key = "trial_type", value = "accuracy") %>%
	mutate(trial_type = as_factor(trial_type))) -> d3


plt <- ggplot(d3, aes(x = trial_type, y = accuracy, fill = pre_crastinate)) 
plt <- plt + geom_boxplot(alpha = 0.5)
plt <- plt + scale_fill_viridis_d("pre-crastinate?")
plt <- plt + scale_y_continuous("accuracy")
plt <- plt + scale_x_discrete("trial type")
plt <- plt + theme_bw()
plt <- plt + theme(legend.position="none") 
ggsave("scratch/acc.pdf", width = 4, height = 3)
ggsave("scratch/acc.png", width = 4, height = 3)





###############################
# Look at eye-movement strategy
###############################

d$x <- ifelse(d$pre_crastinate == "yes", 0.5, -0.5)
X <- cbind(1, d$x)

stan_df <- list(
  N = nrow(d),
  K = ncol(X),
  y = d$opt_strat,
  X = X)

m <- stan(
  file = "beta_simple.stan", 
  data = stan_df,
  chains = 4,
  warmup = 2000,
  iter = 5000,
  refresh = 100
)

samples <- rstan::extract(m)

pred_dat <- tibble(pre_crastinate = c(-0.5, 0.5))

X = as.matrix(pred_dat)
X <- cbind(1, X)

# Look at prior predictions for mmu
mu <- array(0, dim = c(100000, nrow(X)))
for (ii in 1:100000) {
	# generate a random beta from priors
	beta <- as.vector(c(
		rnorm(1, mean=0, sd=.5), 
		rnorm(1, mean=0, sd=.1)
		))
	mu[ii, ] <- plogis(X %*% beta)
}


mu_df <- tibble(
	pre_crastinate = rep(c("no", "yes", "difference"), each = 100000),
	mu  = c(mu[,1], mu[,2], mu[,2] - mu[,1]))

mu_df$pre_crastinate <- as_factor(mu_df$pre_crastinate)
mu_df$pre_crastinate <- fct_relevel(mu_df$pre_crastinate, c("no", "difference", "yes"))



plt <- ggplot(filter(mu_df, pre_crastinate != "difference"), aes(x = mu, fill = pre_crastinate))
plt <- plt + geom_density(alpha = 0.5)
plt <- plt + geom_vline(xintercept = 0, linetype = 2)
plt <- plt + scale_y_continuous(limits = c(0, 10), expand = c(0, 0))
plt <- plt + scale_x_continuous("group mean proportion prior estimates", limits = c(0, 1),, expand = c(0, 0),
	breaks = seq(0,1, 0.25), labels = c("0", "0.25", "0.50", "0.75", "1"))
plt <- plt + scale_fill_viridis_d()
plt <- plt + theme_bw()
plt <- plt + theme(	 
	legend.position="none",
	legend.background = element_rect(size = 0.25, color = "black"))
ggsave("scratch/strat_prior_lines1.pdf", width = 4, height = 3)
ggsave("scratch/strat_prior_lines1.png", width = 4, height = 3)

plt <- plt + geom_vline(alpha = 0.5, data = d, aes(xintercept = opt_strat, colour = pre_crastinate)) +  scale_colour_viridis_d()
ggsave("scratch/strat_prior_lines2.png", width = 4, height = 3)




HPDI(mu[,2] - mu[,1], 0.95)
mean(abs(mu[,2] - mu[,1]) > 0.05)

# Look at posterior predictions for mu

mu <- array(0, dim = c(nrow(samples$beta), nrow(X)))
for (ii in 1:nrow(samples$beta)) {
	mu[ii, ] <- plogis(X %*% samples$beta[ii, ])
}


hpdi <- purrr::map_df(as.tibble(mu), HPDI, prob = 0.90)

# pred_dat$lower = unlist(hpdi[1,])
# pred_dat$upper = unlist(hpdi[2,])
# pred_dat$mean = colMeans(mu)

mu_df <- tibble(
	pre_crastinate = rep(c("no", "yes", "difference"), each = 12000),
	mu  = c(mu[,1], mu[,2], mu[,2] - mu[,1]))
mu_df$pre_crastinate <- as_factor(mu_df$pre_crastinate)
mu_df$pre_crastinate <- fct_relevel(mu_df$pre_crastinate, c("no", "difference", "yes"))

plt <- ggplot(mu_df, aes(x = mu, fill = pre_crastinate))
plt <- plt + geom_density(alpha = 0.5)
plt <- plt + geom_vline(xintercept = 0, linetype = 2)
plt <- plt + scale_x_continuous(
	"group mean proportion posterior estimates", limits = c(-0.1, 0.82), expand = c(0,0))
plt <- plt + scale_y_continuous(limits = c(0, 10), expand = c(0, 0))
plt <- plt + scale_fill_viridis_d()
plt <- plt + theme_bw() + theme(legend.position="none")
ggsave("scratch/strat_diff_2.pdf", width = 4, height = 3)
ggsave("scratch/strat_diff_2.png", width = 4, height = 3)


mu_df <- tibble(
	pre_crastinate = rep(c("no", "yes"), each = 12000),
	mu  = c(mu[,1], mu[,2]))
mu_df$pre_crastinate <- as_factor(mu_df$pre_crastinate)
mu_df$pre_crastinate <- fct_relevel(mu_df$pre_crastinate, c("no", "difference", "yes"))

plt <- ggplot(mu_df, aes(x = mu, fill = pre_crastinate))
plt <- plt + geom_density(alpha = 0.5)
plt <- plt + geom_vline(xintercept = 0, linetype = 2)
plt <- plt + scale_x_continuous(
	"group mean proportion posterior estimates", limits = c(0, 1), expand = c(0, 0), labels = c("0", "0.25", "0.50", "0.75", "1"))
plt <- plt + scale_y_continuous(limits = c(0, 10), expand = c(0, 0))
plt <- plt + scale_fill_viridis_d()
plt <- plt + theme_bw() + theme(legend.position="none")
plt <- plt + geom_vline(alpha = 0.5, data = d, aes(xintercept = opt_strat, colour = pre_crastinate))
plt <- plt + scale_colour_viridis_d()
ggsave("scratch/strat_diff_1.pdf", width = 4, height = 3)
ggsave("scratch/strat_diff_1.png", width = 4, height = 3)
HPDI(mu_diff$diff_prop)



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


x <- seq(0, 1, 0.001)


plt_dat <- tibble(
	pre_crastinate = rep(pred_dat$pre_crastinate, each = length(x)),
	strategy = rep(x, nrow(pred_dat)),
	p = make_predictions(m, pred_dat, x))



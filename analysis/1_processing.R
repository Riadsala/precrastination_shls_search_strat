library(tidyverse)


# 1. Visual Search Data

dat <- read_csv("../data/processed_fix_data.txt", col_types = "fddddddddf")

 # our first job is to remove anybody who did not meet our inclusion criteria for visual search accuracy

dat %>% group_by(person, targ_side_rel) %>%
  summarise(accuracy = mean(accuracy)) -> dat_acc

# now we want to summarise search strategy

# classify every fixation as homo (right), central, or hetro (left)
centralWidth = 64 #change to 1 visual degree
dat$side <- 'central'
dat$x <- dat$x_flip

dat$side[which(dat$x < (0 - centralWidth/2))] = "hard"
dat$side[which(dat$x > (0 + centralWidth/2))] = "easy"
dat$side = as.factor(dat$side)

dat %>% filter(side != "central",
               n < 6, n > 1, 
               targ_side_rel == "absent",
               accuracy == 1) %>% 
  group_by(person, n) %>% 
  summarise(
    prop_hard = mean(side == "hard"), 
    n_trials = length(trial),
    lower = binom.confint(prop_hard*n_trials, n_trials, method = 'wilson')$lower,
    upper = binom.confint(prop_hard*n_trials, n_trials, method = 'wilson')$upper,
    .groups = "drop") -> dat_strat

saveRDS(dat_strat, file="scratch/data_per_person_n.Rda")
write_csv(dat_strat, "scratch/data_per_person_n.txt")

# also calculate aggregate overall strat and put with rt and acc


dat %>% filter(accuracy == 1) %>%
  group_by(person, targ_side_rel) %>% 
  summarise(rt_med = median(rt),
            .groups = "drop") %>%
  rename(rt = "rt_med") %>%
  full_join(dat_acc, by = c("person", "targ_side_rel")) -> dat_rt_acc

dat_strat %>% group_by(person) %>%
  summarise(strat = mean(prop_hard)) %>%
  full_join(dat_rt_acc, by = "person") %>%
  mutate(targ_side_rel = fct_relevel(targ_side_rel, "easy","hard"))-> dat2

saveRDS(dat2, "scratch/data_per_person.Rda")
write_csv(dat2, "scratch/data_per_person.txt")



# 2. Pre-crastination Data


# 3. Personality




# create Big 5 measures
# 
# Extraversion: 1, 6, 11R, 16R, 21, 26R, 31R, 36R, 41, 46, 51R, 56
# Agreeableness: 2, 7, 12R, 17R, 22R, 27, 32, 37R, 42R, 47R, 52, 57
# Conscientiousness: 3R, 8R, 13, 18, 23R, 28R, 33, 38, 43, 48R, 53, 58R
# Negative Emotionality: 4R, 9R, 14, 19, 24R, 29R, 34, 39, 44R, 49R, 54, 59
# Open-Mindedness: 5R, 10, 15, 20, 25R, 30R, 35, 40, 45R, 50R, 55R, 60

d %>% select(SubjID, starts_with("BFI2_")) %>%
  pivot_longer(-SubjID, names_to = "item", values_to = "resp") %>%
  separate(item, into = c("BFI", "item")) %>%
  select(-BFI) %>%
  pivot_wider(SubjID, names_from = "item", values_from = "resp") %>%
  mutate(extraversion = 
           rowMeans(select(., '01', '06', '11r', '21', '26r', '31r', '36r', '41', '46', '51r', '56')),
         agreeableness = 
           rowMeans(select(., '02', '07', '12r', '17r', '22r', '27', '32', '37r', '42r', '47r', '52', '57')),
         conscientiousness = 
           rowMeans(select(., '03r', '08r', '13', '18', '23r', '28r', '33', '38', '43', '48r', '53', '58r')),
         negative_emo = 
           rowMeans(select(., '04r', '09r', '14', '19', '24r', '29r', '34', '39', '44r', '49r', '54', '59')),
         open_mindedness = 
           rowMeans(select(., '05r', '10', '15', '20', '25r', '30r', '35', '40', '45r', '50r', '55r', '60'))) %>%
  select(person = "SubjID", extraversion, agreeableness, conscientiousness, negative_emo, open_mindedness) -> d_BFI

# create procrastination variable
procras_qs_rev <- paste0(procras_qs[which(procras_qs %in% rev_code)], "r")
procras_qs_fwd <- procras_qs[which(!(procras_qs %in% rev_code))]
procras_qs <- c(procras_qs_fwd, procras_qs_rev)
rm(procras_qs_fwd, procras_qs_rev)

d %>% pivot_longer(procras_qs, names_to = "ProscaleID", values_to = "score") %>%
  group_by(SubjID) %>%
  summarise(procrastination =  mean(score)) %>%
  rename(person = "SubjID") %>%
  full_join(d_BFI, by = "person") -> d_out


d %>% select(SubjID, starts_with("BIS11_")) %>%
  pivot_longer(-SubjID, names_to = "item", values_to = "resp") %>%
  separate(item, into = c("BIS", "item")) %>%
  select(-BIS) %>%
  pivot_wider(SubjID, names_from = "item", values_from = "resp") %>%
  mutate(BIStotal = 
           rowMeans(select(., '01r', '02', '03', '04', '05', '06', '07r', '08r', '09r', '10r', '11', '12r', '13r', '14', '15r', '16', '17', '18', '19', '20r', '21', '22', '23', '24', '25', '26', '27', '28', '29r', '30r')),
         attentional_imp = 
           rowMeans(select(., '05', '09r', '11', '20r', '28', '06', '24', '26')),
         motor_imp = 
           rowMeans(select(., '02', '03', '04', '17', '19', '22', '25', '16', '21', '23', '30r')),
         nonplan_imp = 
           rowMeans(select(., '01r', '07r', '08r', '12r', '13r', '14', '10r', '15r', '18', '27', '29r'))) %>%
  select(person = "SubjID", BIStotal, attentional_imp, motor_imp, nonplan_imp)  %>%
  full_join(d_out, by = "person") -> d_out

d_out2 <- full_join(d_out, d %>% rename(person = "SubjID")) %>%
  select(-c(LocationLatitude, LocationLongitude, 
            ResponseId, Finished, Duration_in_Seconds, `Duration (in seconds)`,
            StartTime, EndTime))

write_csv(d_out, "scratch/Personsonality.csv")






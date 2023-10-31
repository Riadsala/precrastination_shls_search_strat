library(tidyverse)


d_search <- read_csv("scratch/dat_rt_acc_strat.csv")
d_precra <- read_csv("scratch/pre_crastinate.csv")
d_person <- read_csv("scratch/personality_q.csv")

# convert search data into wide format

d_search %>% 
  mutate(values =  paste(prop_hard, median_rt, accuracy, sep = "_"), .keep = "unused") %>%
  pivot_wider(names_from = "targ_side_rel", values_from = values) %>%
  separate(absent, into = c("absent_strat", "absent_median_rt", "absent_accuracy"), sep = "_", convert = TRUE) %>%
  separate(easy, into = c("easy_strat", "easy_median_rt", "easy_accuracy"), sep = "_", convert = TRUE) %>%
  separate(hard, into = c("hard_strat", "hard_median_rt", "hard_accuracy"), sep = "_", convert = TRUE) %>%
  rename(search_strat_metric = "absent_strat") %>%
  select(-easy_strat, -hard_strat) -> d_search


# summarise pre-crastination data
d_precra %>% 
  group_by(person, site) %>%
  summarise(
    sensible = sum(choice == "sensible"),
    pre_crastinate = sum(choice != "sensible")) %>%
  mutate(
    classification = case_when(
      sensible > 4 ~ "no",
      sensible < 3 ~ "yes",
      TRUE ~ "intermediate"),
    classification = as_factor(classification),
    classification = fct_relevel(classification, 
                                 "no", "intermediate", "yes")) %>%
  select(person, site,
         n_pc = pre_crastinate, 
         pre_crastinate = "classification") -> d_precra

# join!
d_search %>% full_join(d_precra, by = c("person", "site")) %>%
  full_join(d_person, by = c("person")) %>%
  mutate(search_strat_metric = round(search_strat_metric, 3),
         absent_median_rt = round(absent_median_rt, 3),
         easy_median_rt = round(easy_median_rt, 3),
         hard_median_rt = round(hard_median_rt, 3),
         absent_accuracy = round(absent_accuracy, 3),
         easy_accuracy = round(easy_accuracy, 3),
         hard_accuracy = round(hard_accuracy, 3)) -> d

write_csv(d, "maybe_all_data.csv")

         
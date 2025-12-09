# counts the number of NAs in each column and summarizes it into a dataframe - data looks complete
Na_count_otp_simulated <- otp_simulated %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "column", values_to = "na_count") %>%
  arrange(desc(na_count))

Na_count_ridership_simulated <- ridership_simulated %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "column", values_to = "na_count") %>%
  arrange(desc(na_count))

source("study4_set_up_data.R")
library("broom")

x <- df_long_clean

x %>%
  group_by(scenario, condition) %>%
  summarise(
    mean(R_tot)
    ,sd(R_tot)
    ,mean(M1)
    ,sd(M1)
    )


y <- x %>%
  group_by(scenario)

y %>%
  do(tidy(t.test(R_tot ~ condition, data = .)))

y <- y[which(y$diagnostic12!="NA"),]
y$diagnostic12 <- droplevels(y$diagnostic12)
y %>%
  do(tidy(t.test(R_tot ~ diagnostic12, data = .)))
y$diagnostic12


is.na(y$diagnostic12)

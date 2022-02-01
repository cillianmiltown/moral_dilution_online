rm(list = ls())
library(tidyverse)

# source("../local_analysis/pilot_pre_processing.R")

# read the data file
df <- suppressMessages(read_csv("pilot_data.csv"))
x <- df
# check variable names
variable.names(df)
#View(df)

##### create the dataframe #####

# set up a long data file using the "action choice"
# variable to extract scenario and IV information
# and create an "action_choice" variable

# x <- df %>% pivot_longer(
#   cols = contains("_M_1"),
#   names_to = "scenario_abb",
#   values_to = "moral_perception_measure"
# )

R1 <- x %>%
  select(
  "ResponseId",
  contains("_R_1")) %>%
  pivot_longer(
    cols = contains("_R_1"),
    names_to = "scenario_abb",
    values_to = "MP_R1"
  ) %>% mutate(scenario_abb =
           dplyr::recode(scenario_abb
                         , "A_M_R_1"   = "Alex"
                         , "C_ND2_R_1" = "Charlie"
                         , "F_M_R_1"   = "Francis"
                         , "J_ND1_R_1" = "Jackie"
                         , "R_M_R_1"   = "Robin"
                         , "S_M_R_1"   = "Sam"
           ))


R2 <- x %>%
  select(
    "ResponseId",
    contains("_R_2")) %>%
  pivot_longer(
    cols = contains("_R_2"),
    names_to = "scenario_abb",
    values_to = "MP_R2"
  ) %>% mutate(scenario_abb =
                 dplyr::recode(scenario_abb
                               ,   "A_M_R_2" = "Alex"
                               , "C_ND2_R_2" = "Charlie"
                               ,   "F_M_R_2" = "Francis"
                               , "J_ND1_R_2" = "Jackie"
                               ,   "R_M_R_2" = "Robin"
                               ,   "S_M_R_2" = "Sam"
                 ))


R3 <- x %>%
  select(
    "ResponseId",
    contains("_R_3")) %>%
  pivot_longer(
    cols = contains("_R_3"),
    names_to = "scenario_abb",
    values_to = "MP_R3"
  ) %>% mutate(scenario_abb =
                 dplyr::recode(scenario_abb
                               ,   "A_M_R_3" = "Alex"
                               , "C_ND2_R_3" = "Charlie"
                               ,   "F_M_R_3" = "Francis"
                               , "J_ND1_R_3" = "Jackie"
                               ,   "R_M_R_3" = "Robin"
                               ,   "S_M_R_3" = "Sam"
                 ))

R4 <- x %>%
  select(
    "ResponseId",
    contains("_R_4")) %>%
  pivot_longer(
    cols = contains("_R_4"),
    names_to = "scenario_abb",
    values_to = "MP_R4"
  ) %>% mutate(scenario_abb =
                 dplyr::recode(scenario_abb
                               ,   "A_M_R_4" = "Alex"
                               , "C_ND2_R_4" = "Charlie"
                               ,   "F_M_R_4" = "Francis"
                               , "J_ND1_R_4" = "Jackie"
                               ,   "R_M_R_4" = "Robin"
                               ,   "S_M_R_4" = "Sam"
                 ))

M1 <- x %>%
  select(
    "ResponseId",
    contains("_M_1")) %>%
  pivot_longer(
    cols = contains("_M_1"),
    names_to = "scenario_abb",
    values_to = "M_1"
  ) %>% mutate(scenario_abb =
                 dplyr::recode(scenario_abb
                               ,   "A_M_M_1" = "Alex"
                               , "C_ND2_M_1" = "Charlie"
                               ,   "F_M_M_1" = "Francis"
                               , "J_ND1_M_1" = "Jackie"
                               ,   "R_M_M_1" = "Robin"
                               ,   "S_M_M_1" = "Sam"
                 ))

y <-
  left_join(R1,R2,
            by = c("ResponseId","scenario_abb")) %>%
  left_join(R3,
            by = c("ResponseId","scenario_abb")) %>%
  left_join(R4,
            by = c("ResponseId","scenario_abb")) %>%
  left_join(M1,
            by = c("ResponseId","scenario_abb")) %>%
  left_join(x,
            by = c("ResponseId")) %>%
  mutate(condition=
           dplyr::recode(scenario_abb
                         , "Alex" = "Diagnostic"
                         , "Sam" = "Diagnostic"
                         , "Robin" = "Diagnostic"
                         , "Francis" = "Diagnostic"
                         , "Jackie" = "Non-Diagnostic"
                         , "Charlie" = "Non-Diagnostic"))


x <- y

x$scenario_abb <- as.factor(x$scenario_abb)
x$condition <- as.factor(x$condition)


table(x$scenario_abb)

full_wide <- df
full_long <- x
df <- x


x <- x[which(((x$attn_chk_2_Q=="Soccer"|x$attn_chk_2_Q=="Swimming")==FALSE&x$attn_chk_1Q!="None of the above")==FALSE),]

full_long_clean <- x

as.data.frame(x)
head(as.data.frame(x))


write.csv(full_wide, "data/pilot_data_wide.csv", row.names = FALSE)
write.csv(full_long, "data/pilot_data_long.csv", row.names = FALSE)
write.csv(full_long_clean, "data/pilot_data_long_clean.csv", row.names = FALSE)


rm(df,M1,R1,R2,R3,R4,x,y)




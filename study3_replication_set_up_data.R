rm(list = ls())
library(tidyverse)

# source("../local_analysis/pilot_pre_processing.R")

# read the data file

df <- suppressMessages(read_csv("../data/study_3_replication.csv"))
#df <- suppressMessages(read_csv("../../data/study_5.csv"))


# check variable names
variable.names(df)
#View(df)

#df <-
# y <- df[which(df$age==7708),]
# y <- y %>%
#   mutate(age=
#            dplyr::recode(age
#                          , "7708" = "45"
#                          ))
# y1 <- y
# y <- df[which(df$age==47906),]
# y <- y %>%
#   mutate(age=
#            dplyr::recode(age
#                          , "47906" = "47"
#            ))
# y2 <- y
# y <- df[which(df$age==3),]
# y <- y %>%
#   mutate(age=
#            dplyr::recode(age
#                          , "3" = "30"
#            ))
# y3 <- y
#
# z <- df
# z <- z[which(z$age!=7708),]
# z <- z[which(z$age!=47906),]
# z <- z[which(z$age!=3),]
#
# df <- rbind(z,y1,y2,y3)

df$age <- as.numeric(df$age)

#rm(y,y1,y2,y3,z)
x <- df
##### create the dataframe #####

# set up a long data file using the "action choice"
# variable to extract scenario and IV information
# and create an "action_choice" variable

# x <- df %>% pivot_longer(
#   cols = contains("_M_1"),
#   names_to = "scenario_abb",
#   values_to = "moral_perception_measure"
# )


x$scenario <- rep(NA)
x$condition <- rep(NA)
x$diagnostic12 <- rep(NA)
x$valence <- rep(NA)

# x$R1 <- rep(NA)
# x$R2 <- rep(NA)
# x$R3 <- rep(NA)
# x$R4 <- rep(NA)
# x$M1 <- rep(NA)

#### Sam ####
sam <- x %>%
  select(
    "ResponseId"
    , starts_with("S_")
    , "scenario","condition","diagnostic12","valence"
  )



ND1 <- sam[which(is.na(sam$S_G_ND1_R_1)==FALSE),]
ND1$scenario <- rep("sam")

ND1$condition <- rep("non-diagnostic")
ND1$diagnostic12 <- rep("ND1")
ND1 <- Filter(function(x)!all(is.na(x)), ND1)
ND1 <- `colnames<-`(ND1,
             c("ResponseId", "R1","R2","R3","R4","M1",
               "scenario","condition","diagnostic12","valence"))


ND2 <- sam[which(is.na(sam$S_G_ND2_R_1)==FALSE),]
ND2$scenario <- rep("sam")

ND2$condition <- rep("non-diagnostic")
ND2$diagnostic12 <- rep("ND2")
ND2 <- Filter(function(x)!all(is.na(x)), ND2)
ND2 <- `colnames<-`(ND2,
             c("ResponseId", "R1","R2","R3","R4","M1",
               "scenario","condition","diagnostic12","valence"))



M <- sam[which(is.na(sam$S_G_M_M_1)==FALSE),]
M$scenario <- rep("sam")

M$condition <- rep("diagnostic")
M$diagnostic12 <- rep("NA")
M <- Filter(function(x)!all(is.na(x)), M)
M <- `colnames<-`(M,
             c("ResponseId", "R1","R2","R3","R4","M1",
               "scenario","condition","diagnostic12","valence"))

sam <- rbind.data.frame(ND1, ND2, M)
sam$valence <- rep("good")


#### Robin ####
robin <- x %>%
  select(
    "ResponseId"
    , starts_with("R_")
    , "scenario","condition","diagnostic12","valence"
  )

ND1 <- robin[which(is.na(robin$R_G_ND1_R_1)==FALSE),]
ND1$scenario <- rep("robin")

ND1$condition <- rep("non-diagnostic")
ND1$diagnostic12 <- rep("ND1")
ND1 <- Filter(function(x)!all(is.na(x)), ND1)
ND1 <- `colnames<-`(ND1,
                    c("ResponseId", "R1","R2","R3","R4","M1",
                      "scenario","condition","diagnostic12","valence"))


ND2 <- robin[which(is.na(robin$R_G_ND2_R_1)==FALSE),]
ND2$scenario <- rep("robin")

ND2$condition <- rep("non-diagnostic")
ND2$diagnostic12 <- rep("ND2")
ND2 <- Filter(function(x)!all(is.na(x)), ND2)
ND2 <- `colnames<-`(ND2,
                    c("ResponseId", "R1","R2","R3","R4","M1",
                      "scenario","condition","diagnostic12","valence"))



M <- robin[which(is.na(robin$R_G_M_R_1)==FALSE),]
M$scenario <- rep("robin")

M$condition <- rep("diagnostic")
M$diagnostic12 <- rep("NA")
M <- Filter(function(x)!all(is.na(x)), M)
M <- `colnames<-`(M,
                  c("ResponseId", "R1","R2","R3","R4","M1",
                    "scenario","condition","diagnostic12","valence"))


robin <- rbind.data.frame(ND1, ND2, M)
robin$valence <- rep("good")


#### Francis ####
francis <- x %>%
  select(
    "ResponseId"
    , starts_with("F_")
    , "scenario","condition","diagnostic12","valence"
  )

ND1 <- francis[which(is.na(francis$F_B_ND1_R_1)==FALSE),]
ND1$scenario <- rep("francis")

ND1$condition <- rep("non-diagnostic")
ND1$diagnostic12 <- rep("ND1")
ND1 <- Filter(function(x)!all(is.na(x)), ND1)
ND1 <- `colnames<-`(ND1,
                    c("ResponseId", "R1","R2","R3","R4","M1",
                      "scenario","condition","diagnostic12","valence"))


ND2 <- francis[which(is.na(francis$F_B_ND2_R_1)==FALSE),]
ND2$scenario <- rep("francis")

ND2$condition <- rep("non-diagnostic")
ND2$diagnostic12 <- rep("ND2")
ND2 <- Filter(function(x)!all(is.na(x)), ND2)
ND2 <- `colnames<-`(ND2,
                    c("ResponseId", "R1","R2","R3","R4","M1",
                      "scenario","condition","diagnostic12","valence"))



M <- francis[which(is.na(francis$F_B_M_R_1)==FALSE),]
M$scenario <- rep("francis")

M$condition <- rep("diagnostic")
M$diagnostic12 <- rep("NA")
M <- Filter(function(x)!all(is.na(x)), M)
M <- `colnames<-`(M,
                  c("ResponseId", "R1","R2","R3","R4","M1",
                    "scenario","condition","diagnostic12","valence"))


francis <- rbind.data.frame(ND1, ND2, M)
francis$valence <- rep("bad")


#### Alex ####
alex <- x %>%
  select(
    "ResponseId"
    , starts_with("A_")
    , "scenario","condition","diagnostic12","valence"
  )

ND1 <- alex[which(is.na(alex$A_B_ND1_R_1)==FALSE),]
ND1$scenario <- rep("alex")

ND1$condition <- rep("non-diagnostic")
ND1$diagnostic12 <- rep("ND1")
ND1 <- Filter(function(x)!all(is.na(x)), ND1)
ND1 <- `colnames<-`(ND1,
                    c("ResponseId", "R1","R2","R3","R4","M1",
                      "scenario","condition","diagnostic12","valence"))


ND2 <- alex[which(is.na(alex$A_B_ND2_R_1)==FALSE),]
ND2$scenario <- rep("alex")

ND2$condition <- rep("non-diagnostic")
ND2$diagnostic12 <- rep("ND2")
ND2 <- Filter(function(x)!all(is.na(x)), ND2)
ND2 <- `colnames<-`(ND2,
                    c("ResponseId", "R1","R2","R3","R4","M1",
                      "scenario","condition","diagnostic12","valence"))



M <- alex[which(is.na(alex$A_B_M_R_1)==FALSE),]
M$scenario <- rep("alex")

M$condition <- rep("diagnostic")
M$diagnostic12 <- rep("NA")
M <- Filter(function(x)!all(is.na(x)), M)
M <- `colnames<-`(M,
                  c("ResponseId", "R1","R2","R3","R4","M1",
                    "scenario","condition","diagnostic12","valence"))


alex <- rbind.data.frame(ND1, ND2, M)
alex$valence <- rep("bad")


##### Merge them all #####

x <- rbind.data.frame(sam, robin, francis, alex)

x <- x %>% left_join(df %>% select("ResponseId", "age","gender"#,"Sample"
                                   ,"attn_chk_1Q","attn_chk_2_Q"
                                   ),
                by = "ResponseId")#


x$scenario     <- as.factor(x$scenario)
x$condition    <- as.factor(x$condition)
x$diagnostic12 <- as.factor(x$diagnostic12)
x$valence      <- as.factor(x$valence)


x$R_tot <- rowMeans(
  x[2:5]
)

x$M1R_tot <- scale(scale(x$R_tot)+scale(x$M1))


df_long <- x

x <- df_long


x$att1_fail <- x$attn_chk_1Q!="7"
x$att2_fail <- (x$attn_chk_2_Q=="2"|x$attn_chk_2_Q=="5")==FALSE

#table(x$att1_fail,x$att2_fail)
x$att_fail <- x$att1_fail==T & x$att2_fail==T

#table(x$att_fail)

df_long_clean <- x[which(x$att_fail==FALSE),]
table(df_long_clean$attn_chk_1Q,df_long_clean$attn_chk_2_Q)


attn_missing <- x[which(is.na(x$att_fail)),]

# x <- df_long
# #x <- x[as.numeric(ave(x$ResponseId, x$ResponseId, FUN=length)) >= 4, ]
# x <- x[as.numeric(ave(x$ResponseId, x$ResponseId, FUN=length)) >= 4, ]
# #sum(x$attn_chk_1Q==7)
# x <- x[which(((x$attn_chk_2_Q=="2"|x$attn_chk_2_Q=="5")==FALSE&x$attn_chk_1Q!="7")==FALSE),]
# table(x$attn_chk_1Q)
# table(x$attn_chk_2_Q)
# # #x <-
# # x[which(((x$attn_chk_2_Q=="2"|x$attn_chk_2_Q=="5")==FALSE | x$attn_chk_1Q=="7")),]
# # x[which(((x$attn_chk_2_Q!="2"&x$attn_chk_2_Q!="5")==FALSE)),]
# # x[which(((x$attn_chk_2_Q!="2"&x$attn_chk_2_Q!="5")==FALSE | x$attn_chk_1Q=="7")),]



#df_long_clean <- x




#df_long <- x
# x <- df_long
# #x <- x[as.numeric(ave(x$ResponseId, x$ResponseId, FUN=length)) >= 4, ]
# x <- x[as.numeric(ave(x$ResponseId, x$ResponseId, FUN=length)) >= 4, ]
# #sum(x$attn_chk_1Q==7)
# x <- x[which(((x$attn_chk_2_Q=="2"|x$attn_chk_2_Q=="5")==FALSE&x$attn_chk_1Q!="7")==TRUE),]
# table(x$attn_chk_1Q)
# table(x$attn_chk_2_Q)

# #x <-
# x[which(((x$attn_chk_2_Q=="2"|x$attn_chk_2_Q=="5")==FALSE | x$attn_chk_1Q=="7")),]
# x[which(((x$attn_chk_2_Q!="2"&x$attn_chk_2_Q!="5")==FALSE)),]
# x[which(((x$attn_chk_2_Q!="2"&x$attn_chk_2_Q!="5")==FALSE | x$attn_chk_1Q=="7")),]



df_long_failed <- x[which(x$att_fail==TRUE),]




sam$R_tot <- rowMeans(
  sam[2:5]
)
robin$R_tot <- rowMeans(
  robin[2:5]
)
francis$R_tot <- rowMeans(
  francis[2:5]
)
alex$R_tot <- rowMeans(
  alex[2:5]
)




names(sam)[2:11]
names(sam)[2:11] <- paste0("sam_", names(sam)[2:11] )
names(robin)[2:11] <- paste0("robin_", names(robin)[2:11] )
names(francis)[2:11] <- paste0("francis_", names(francis)[2:11] )
names(alex)[2:11] <- paste0("alex_", names(alex)[2:11] )


x <- x %>% left_join(df %>% select("ResponseId", "age","gender"),
                     by = "ResponseId")#


df_wide <- left_join(sam,robin, by="ResponseId") %>%
  left_join(francis, by="ResponseId") %>%
  left_join(alex, by="ResponseId")%>%
  left_join(df %>% select("ResponseId", "age","gender"
                          , "attn_chk_1Q", "attn_chk_2_Q"),
                                                by = "ResponseId")

df_wide_clean <-
df_wide[which(
  is.na(df_wide$alex_M1)==FALSE &
    is.na(df_wide$francis_M1)==FALSE &
    is.na(df_wide$sam_M1)==FALSE &
    is.na(df_wide$robin_M1)==FALSE
  ),]


# attention_fun <- function(data){
#   x <- data
#   x <- x[which(((x$attn_chk_2_Q=="2"|x$attn_chk_2_Q=="5")==FALSE&x$attn_chk_1Q!="7")==FALSE),]
#   x
# }
#df_wide_clean <-

x <- df_wide

x$att1_fail <- x$attn_chk_1Q!="7"
x$att2_fail <- (x$attn_chk_2_Q=="2"|x$attn_chk_2_Q=="5")==FALSE

#table(x$att1_fail,x$att2_fail)
x$att_fail <- x$att1_fail==T & x$att2_fail==T

#table(x$att_fail)

df_wide_clean <- x[which(x$att_fail==FALSE),]

#df_wide_clean <- attention_fun(df_wide_clean)

# #x <-
# x[which(((x$attn_chk_2_Q=="2"|x$attn_chk_2_Q=="5")==FALSE&x$attn_chk_1Q!="7")==FALSE),]
# sum(((x$attn_chk_2_Q=="2"|x$attn_chk_2_Q=="5")&x$attn_chk_1Q!="7"))
# sum(((x$attn_chk_2_Q=="2"|x$attn_chk_2_Q=="5")))#&x$attn_chk_1Q!="7"))
# sum(((x$attn_chk_2_Q=="2")))#&x$attn_chk_1Q!="7"))
# sum(((x$attn_chk_2_Q=="5")))#&x$attn_chk_1Q!="7"))
#
# sum(((x$attn_chk_2_Q!="2"&x$attn_chk_2_Q!="5")))
# sum(x$attn_chk_1Q!="7")
# sum(((x$attn_chk_2_Q!="2"&x$attn_chk_2_Q!="5"&x$attn_chk_1Q!="7")))

# table(x$ResponseId)
#
# x$R_tot
#
# desnum::t_paragraph(x$M1,x$condition, "dilution")
# desnum::t_paragraph(x$R_tot,x$condition, "dilution")
#
# lme4::lmer()
#
#
#
# R1 <- sam %>%
#   select()
#
# R1 <- x %>%
#   select(
#   "ResponseId",
#   contains("_R_1")) %>%
#   pivot_longer(
#     cols = contains("_R_1"),
#     names_to = "scenario_abb",
#     values_to = "MP_R1"
#   ) %>% mutate(scenario_abb =
#            dplyr::recode(scenario_abb
#                          , "A_M_R_1"   = "Alex"
#                          , "C_ND2_R_1" = "Charlie"
#                          , "F_M_R_1"   = "Francis"
#                          , "J_ND1_R_1" = "Jackie"
#                          , "R_M_R_1"   = "Robin"
#                          , "S_M_R_1"   = "Sam"
#            ))
#
#
# R2 <- x %>%
#   select(
#     "ResponseId",
#     contains("_R_2")) %>%
#   pivot_longer(
#     cols = contains("_R_2"),
#     names_to = "scenario_abb",
#     values_to = "MP_R2"
#   ) %>% mutate(scenario_abb =
#                  dplyr::recode(scenario_abb
#                                ,   "A_M_R_2" = "Alex"
#                                , "C_ND2_R_2" = "Charlie"
#                                ,   "F_M_R_2" = "Francis"
#                                , "J_ND1_R_2" = "Jackie"
#                                ,   "R_M_R_2" = "Robin"
#                                ,   "S_M_R_2" = "Sam"
#                  ))
#
#
# R3 <- x %>%
#   select(
#     "ResponseId",
#     contains("_R_3")) %>%
#   pivot_longer(
#     cols = contains("_R_3"),
#     names_to = "scenario_abb",
#     values_to = "MP_R3"
#   ) %>% mutate(scenario_abb =
#                  dplyr::recode(scenario_abb
#                                ,   "A_M_R_3" = "Alex"
#                                , "C_ND2_R_3" = "Charlie"
#                                ,   "F_M_R_3" = "Francis"
#                                , "J_ND1_R_3" = "Jackie"
#                                ,   "R_M_R_3" = "Robin"
#                                ,   "S_M_R_3" = "Sam"
#                  ))
#
# R4 <- x %>%
#   select(
#     "ResponseId",
#     contains("_R_4")) %>%
#   pivot_longer(
#     cols = contains("_R_4"),
#     names_to = "scenario_abb",
#     values_to = "MP_R4"
#   ) %>% mutate(scenario_abb =
#                  dplyr::recode(scenario_abb
#                                ,   "A_M_R_4" = "Alex"
#                                , "C_ND2_R_4" = "Charlie"
#                                ,   "F_M_R_4" = "Francis"
#                                , "J_ND1_R_4" = "Jackie"
#                                ,   "R_M_R_4" = "Robin"
#                                ,   "S_M_R_4" = "Sam"
#                  ))
#
# M1 <- x %>%
#   select(
#     "ResponseId",
#     contains("_M_1")) %>%
#   pivot_longer(
#     cols = contains("_M_1"),
#     names_to = "scenario_abb",
#     values_to = "M_1"
#   ) %>% mutate(scenario_abb =
#                  dplyr::recode(scenario_abb
#                                ,   "A_M_M_1" = "Alex"
#                                , "C_ND2_M_1" = "Charlie"
#                                ,   "F_M_M_1" = "Francis"
#                                , "J_ND1_M_1" = "Jackie"
#                                ,   "R_M_M_1" = "Robin"
#                                ,   "S_M_M_1" = "Sam"
#                  ))
#
# y <-
#   left_join(R1,R2,
#             by = c("ResponseId","scenario_abb")) %>%
#   left_join(R3,
#             by = c("ResponseId","scenario_abb")) %>%
#   left_join(R4,
#             by = c("ResponseId","scenario_abb")) %>%
#   left_join(M1,
#             by = c("ResponseId","scenario_abb")) %>%
#   left_join(x,
#             by = c("ResponseId")) %>%
#   mutate(condition=
#            dplyr::recode(scenario_abb
#                          , "Alex" = "Diagnostic"
#                          , "Sam" = "Diagnostic"
#                          , "Robin" = "Diagnostic"
#                          , "Francis" = "Diagnostic"
#                          , "Jackie" = "Non-Diagnostic"
#                          , "Charlie" = "Non-Diagnostic"))
#
#
# x <- y
#
# x$scenario_abb <- as.factor(x$scenario_abb)
# x$condition <- as.factor(x$condition)
#
#
# table(x$scenario_abb)
#
# full_wide <- df
# full_long <- x
# df <- x
#
#
# x <- x[which(((x$attn_chk_2_Q=="Soccer"|x$attn_chk_2_Q=="Swimming")==FALSE&x$attn_chk_1Q!="None of the above")==FALSE),]
#
# full_long_clean <- x
#
# as.data.frame(x)
# head(as.data.frame(x))
#
#
write.csv(df_wide,       "data/study3_rep_data_wide.csv", row.names = FALSE)
write.csv(df_long,       "data/study3_rep_data_long.csv", row.names = FALSE)
write.csv(df_long_clean, "data/study3_rep_data_long_clean.csv", row.names = FALSE)
write.csv(df_wide_clean, "data/study3_rep_data_wide_clean.csv", row.names = FALSE)
write.csv(df_long_failed, "data/study3_rep_data_long_failed.csv", row.names = FALSE)
#
#
rm(df,M,ND1, ND2,x, alex, francis, robin, sam)
#
#
#

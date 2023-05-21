

#install.packages(c("colorspace", "fansi", "fs", "MASS", "mnormt", "Rcpp", "TOSTER", "vctrs", "yaml"))
getwd()
#setwd("manuscript_prep")
#setwd("manuscript_prep")
#setwd("E:/Home/Dropbox/College/research/collab/Moral_Dilution/moral_dilution_online/manuscript_prep")


#source("~/Dropbox/College/research/Research_general/cog_load/moral_dumbfounding_and_cognitive_load/read_and_sort_raw_data.R")
#source("~/Dropbox/College/research/Research_general/cog_load/moral_dumbfounding_and_cognitive_load/load_study6_data.R")

# Chunk 1: S1setup
knitr::opts_chunk$set(echo = FALSE, include = FALSE)
# knitr::opts_chunk$set(eval = TRUE, echo = TRUE)
#knitr::opts_chunk$set(include = FALSE)
# Chunk 2: S1load_libraries_cogload
rm(list = ls())
library(citr)
#install.packages("sjstats")
library(plyr)
library(foreign)
library(car)
library(desnum)
library(ggplot2)
library(extrafont)
#devtools::install_github("crsh/papaja")
library(papaja)
#library("dplyr")
library("afex")
library("tibble")
library(scales)
#install.packages("metap")
library(metap)
library(pwr)
library(lsr)
#install.packages("sjstats")
library(sjstats)
library(DescTools)
#inatall.packages("ggstatsplot")
#library(ggstatsplot)
library(VGAM)
library(nnet)
library(mlogit)
library(reshape2)
#install.packages("powerMediation")
library("powerMediation")
library("ggpubr")
# library(rstatix)
#source("load_all_data.R")
#devtools::install_github("benmarwick/wordcountaddin")
#library(wordcountaddin)
#wordcountaddin::text_stats("cogload_1to5_25Sept19.Rmd")
#setwd("manuscript_prep")
getwd()
#source("~/Dropbox/College/research/Research_general/cog_load/moral_dumbfounding_and_cognitive_load/read_and_sort_raw_data.R")
#source("~/Dropbox/College/research/Research_general/cog_load/moral_dumbfounding_and_cognitive_load/load_study6_data.R")


rm(list = ls())
df3 <- read.csv("../data/study1_data_long.csv")
df3 <- read.csv("../data/study1_data_wide.csv")
x <- read.csv("../data/study1_data_long_clean.csv")
study1 <- x

df3 <- read.csv("../data/study4_data_long.csv")
df1 <- read.csv("../data/study4_data_wide.csv")
x <- read.csv("../data/study4_data_long_clean.csv")
study2 <- x

df3 <- read.csv("../data/study5_data_long.csv")
df1 <- read.csv("../data/study5_data_wide.csv")
x <- read.csv("../data/study5_data_long_clean.csv")
study3 <- x

df3 <- read.csv("../data/study4_rep_data_long.csv")
df1 <- read.csv("../data/study4_rep_data_wide.csv")
x <- read.csv("../data/study4_rep_data_long_clean.csv")
study4 <- x

df3 <- read.csv("../data/study6_data_long.csv")
df1 <- df3
x <- read.csv("../data/study6_data_long_clean.csv")
study5 <- x


####

#cbind.data.frame(
variable.names(study1)
variable.names(study2)
variable.names(study3)
variable.names(study4)
variable.names(study5)

study1$valence <- rep("bad",length(study1$gender))
study2$valence <- rep("good",length(study2$gender))
study4$valence <- rep("good",length(study4$gender))


study1_mini <- study1 %>% select(
  M1,R_tot,condition,valence
)


study2_mini <- study2 %>% select(
  M1,R_tot,condition,valence
)


study3_mini <- study3 %>% select(
  M1,R_tot,condition,valence
)


study4_mini <- study4 %>% select(
  M1,R_tot,condition,valence
)


study5_mini <- study5 %>% select(
  M1,R_tot,condition,valence
)


all_studies_mini <- rbind.data.frame(
  study1_mini
  ,study2_mini
  ,study3_mini
  ,study4_mini
  ,study5_mini
)


x <- all_studies_mini

x <- study5_mini

x$Valence <- dplyr::recode(x$valence
                           , "good" = "Good"
                           , "bad" = "Bad")

paste(x$valence,x$condition)

x$color1 <- paste(x$valence,x$condition)
table(x$color1)

mps4plot <-
  ggplot(x,aes(x=condition,y=R_tot,fill=factor(valence)))+
  geom_violin() +
  stat_summary(fun=mean, geom="point", shape=23, size=2)+

  #  geom_dotplot(binaxis='y', stackdir='center', dotsize=.05)+
  # violin plot with jittered points
  # 0.2 : degree of jitter in x direction
  # geom_jitter(shape=16
  #             , position=position_jitter(0.15)
  #             , size=.1
  #             , color="dark grey") +
  geom_sina(shape=16
            , position=position_jitter(0.015)
            , size=.1
            , aes(color=factor(valence))
            ,jitter_y=TRUE
  )+
#  scale_color_manual(values = c("#ff5252","#1ca3ec"))+
  scale_color_manual(values = c(
    "#ff5252"
    #'#5e3c99'
    #'#990099'
    #,'#660099'
    #,"darkgreen"
    ,"#1ca3ec"
    #,"yellow"
  ))+
  scale_fill_manual(values = c(
    #"#ff5252" #ffc6c3
    '#ffebea'
    ,'#d2e6f1' #,"#96c5de"
    #,'#aed2ff'
    #,"darkgreen"
    #,"#1ca3ec"
    #,"yellow"
  ))+
  geom_boxplot(width=0.1,outlier.shape = NA)+
  xlab("Condition") +
  ylab("MPS-4") +
  facet_grid(cols = vars(Valence)) +
  theme_bw() +
  theme(panel.border = element_blank(),
        axis.line = element_line(size = .2),
        strip.background  = element_blank(),
        panel.grid = element_blank(),
        plot.title=element_text(#family="Times",
          size=12
        ),
        legend.text=element_text(#family="Times",
          size=8
        ),
        legend.title=element_text(#family="Times",
          size=10
        ),
        axis.text=element_text(#family="Times",
          colour = "black",
          size=8
        ),
        axis.ticks.x = element_blank(),
        axis.title=element_text(#family="Times",
          size=12
        ),
        strip.text=element_text(#family = "Times",
          size = 12
        ),
        # strip.background = element_rect(fill = "white"),
        legend.position="none")




M1plot <-
  ggplot(x,aes(x=condition,y=M1,fill=factor(valence)
               #, color=factor(condition)
               ))+
  geom_violin() +
  stat_summary(fun=mean, geom="point", shape=23, size=2)+

  #  geom_dotplot(binaxis='y', stackdir='center', dotsize=.05)+
  # violin plot with jittered points
  # 0.2 : degree of jitter in x direction
  # geom_jitter(shape=16
  #             , position=position_jitter(0.15)
  #             , size=.1
  #             , color="dark grey") +
  geom_sina(shape=16
            , position=position_jitter(0.015)
            , size=.1
            , aes(color=factor(valence))
            ,jitter_y=TRUE
  )+
  scale_color_manual(values = c(
    "#ff5252"
    #'#5e3c99'
    #'#990099'
    #,'#660099'
    #,"darkgreen"
    ,"#1ca3ec"
    #,"yellow"
    ))+
  scale_fill_manual(values = c(
    #"#ff5252" #ffc6c3
    '#ffebea'
    ,'#d2e6f1' #,"#96c5de"
    #,'#aed2ff'
    #,"darkgreen"
    #,"#1ca3ec"
    #,"yellow"
    ))+
  geom_boxplot(width=0.1,outlier.shape = NA)+
  xlab("Condition") +
  ylab("MM-1")+
  facet_grid(cols = vars(Valence)) +
  theme_bw() +
  theme(panel.border = element_blank(),
        axis.line = element_line(size = .2),
        strip.background  = element_blank(),
        panel.grid = element_blank(),
        plot.title=element_text(#family="Times",
          size=12
        ),
        #legend.position="right",
        legend.text=element_text(#family="Times",
          size=8
        ),
        legend.title=element_text(#family="Times",
          size=10
        ),
        axis.text=element_text(#family="Times",
          colour = "black",
          size=8
        ),
        axis.ticks.x = element_blank(),
        axis.title=element_text(#family="Times",
          size=12
        ),
        strip.text=element_text(#family = "Times",
          size = 12
        ),
        # strip.background = element_rect(fill = "white"),
        legend.position="none")

#M1plot


figure <- ggarrange(mps4plot
                    , M1plot
                    #,labels = c("A")
                    , ncol = 1
                    , nrow = 2
                    #, widths = c(0.485, 0.5)
)
figure

ggsave('combined_plots.png', figure, bg='transparent')

ggsave('study1.png', figure, width = 4.505, height = 6.16, bg='transparent')
ggsave('study2.png', figure, width = 4.505, height = 6.16, bg='transparent')
ggsave('study3.png', figure, bg='transparent')
ggsave('study4.png', figure, width = 4.505, height = 6.16, bg='transparent')
ggsave('study5.png', figure, bg='transparent')







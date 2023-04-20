#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                                                                                ##
# Whippo et al. 2023 Urchin Consumption Stats/Figures                            ##
# Script Created 2023-04-10                                                      ##
# Last updated 2023-04-10                                                        ##
# Data source: Ross Whippo PhD Dissertation                                      ##
# R code prepared by Ross Whippo                                                 ##
#                                                                                ##
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# SUMMARY:
# Script to create figures for manuscript of purple urchin-pycnopodia nonconsumptive
# interactions. Experiments run in summer of 2020 and 2021 at the Friday Harbor
# Laboratories by Ross Whippo, PhD candidate at University of Oregon. 

# Required Files:
# trials2020_QAQC.csv
# trials2021_QAQC.csv
# trials2021_trackerDist_QAQC.csv
# algal_consumption.csv


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# TABLE OF CONTENTS                                                            ####
#                                                                                 +
# LOAD PACKAGES                                                                   +
# READ IN DATA                                                                    +
# 2020 URCHIN FEEDING                                                             +
# 2021 URCHIN MOVEMENT                                                            +
#                                                                                 +
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# LOAD PACKAGES                                                                ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(tidyverse) # data manipulation, tidying 
library(viridis) # color-blind-friendly palette 
library(ggpubr) # additional visualizations 

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# READ IN DATA                                                                 ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

trials2020_Q <- read_csv("Data/2020/trials2020_QAQC.csv", 
                         col_types = cols(trial = col_character(), 
                                          bin = col_character(), 
                                          diameter = col_double()))

trials2021_Q <- read_csv("Data/2021/trials2021_QAQC.csv", 
                         col_types = cols(date = col_character(),  
                                          location = col_character(), 
                                          movement = col_character(), 
                                          timeBegin = col_character(), 
                                          timeEnd = col_character()))

trials2021_trackerDist_QAQC <- read_csv("Data/2021/trials2021_trackerDist_QAQC.csv")

algal_consumption <- read_csv("Data/2021/algal_consumption.csv")


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 2020 URCHIN FEEDING                                                          ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

str(trials2020_Q)

# Experimental structure:

# To detect indirect effects of Pycnopodia on purple urchin feeding rates, the experiment
# tracked the consumption of kelp by urchins across 5 paired replicates with two treatments
# (exposed to Pycno, not exposed to Pycno) for between 51 and 69 hours. Urchins were
# held individually in small containers within one of two larger seawater tanks that either
# had 'Pycno smell' or not. Kelp was fed to the urchins as small standardized disks
# and consumption across the trial was calculated as the number of disks they ate between
# each time point at which they were checked (every six hours during daylight, twelve hours
# between next day). Values of disks consumed are transformed into biomass below in the 
# script based on weights of 'test disks' of kelp (i.e.: 1 disk = 0.341 g kelp tissue).

# Primary questions:

# Q1
# Did the urchins consume different amounts of kelp through time based on their treatment?


####################################

####################
#      ##     ##   #
#    ##  ##    #   #
#   #     #    #   # 
#   #     #    #   #
#   #  #  #    #   #
#    #  ##     #   #
#     ### #   ###  #
#                  #
#################### 

# Addressing Q1 : total consumption differences across the experiment between treatments

per_urchin_hourly <- trials2020_Q %>%
  filter(timepoint != 0) %>%
  group_by(trial, bin, tank, ID, pycno) %>%
  mutate(hoursbetween = case_when(timepoint == 1 ~ 3,
                                  timepoint == 2 ~ 6,
                                  timepoint == 3 ~ 12,
                                  timepoint == 4 ~ 6,
                                  timepoint == 5 ~ 6,
                                  timepoint == 6 ~ 12,
                                  timepoint == 7 ~ 6,
                                  timepoint == 8 ~ 6,
                                  timepoint == 9 ~ 12)) %>%
  mutate(hourstotal = case_when(timepoint == 1 ~ 3,
                                 timepoint == 2 ~ 9,
                                 timepoint == 3 ~ 21,
                                 timepoint == 4 ~ 27,
                                 timepoint == 5 ~ 33,
                                 timepoint == 6 ~ 45,
                                 timepoint == 7 ~ 51,
                                 timepoint == 8 ~ 57,
                                 timepoint == 9 ~ 69)) %>%
  mutate(grams = consumed * 0.341) %>%
  mutate(gramsperhour = grams/hoursbetween) %>%
  mutate(loggramsperhour = log10(gramsperhour + 1)) %>%
  unite(trialtank, trial, tank, sep = "", remove = FALSE)


# FIGURE - hourly consumed per urchin across all trials 
feeding_plot <- ggplot(per_urchin_hourly, aes(x = hourstotal, y = gramsperhour, color = pycno)) +
  scale_color_viridis(name = "Pycnopodia Cue", discrete = TRUE,
                      begin = 0.3,
                      end = 0.7,
                      option = "magma") +
  geom_jitter(size = 2, width = 0.5, alpha = 0.25) +
  geom_smooth(method = "lm", linewidth = 2) +
  scale_y_continuous(name = "Kelp Consumed (g/hr)") +
  xlab("Hours") +
  theme_bw(base_size = 15) +
  theme(axis.title.y = element_text(vjust = 3)) +
  annotate("rect", xmin = 24, xmax = 36, ymin = -0.03, ymax = 0.48,
             alpha = .01, color = "red", fill = "grey", linetype = "dotted",
           linewidth = 0.8)

# FIGURE - hourly consumed per urchin across all trials with standard error at TP 4
consumed_plot <- per_urchin_hourly %>%
  mutate(Treatment = ifelse(pycno == 'yes', "Pycno", "Control")) %>%
  filter(timepoint == 4) %>%
  ggplot(aes(x = Treatment, y = gramsperhour, color = Treatment)) + 
  ggtitle("27 hours") +
  geom_point(col = "grey", size = 3) +
  scale_color_viridis(discrete = TRUE,
                      begin = 0.3,
                      end = 0.7,
                      option = "magma") +
  stat_summary(
    geom = "point",
    fun = "mean",
    size = 6,
    shape = 19
  ) +
  geom_errorbar(stat="summary", fun.data="mean_se", size = 1) +
  ylim(-0.06, 0.63) +
  labs(y = "", x = "Treatment") +
  theme_minimal(base_size = 15) +
  theme(axis.title.y = element_text(vjust = 3), axis.text.x=element_blank()) +
  theme(legend.position = "none") +
  annotate("text", x = 1.5, y = 0.62, label = "p = 0.004")


# FIGURE - hourly consumed per urchin across all trials with standard error at TP 5
consumed_plot2 <- per_urchin_hourly %>%
  mutate(Treatment = ifelse(pycno == 'yes', "Pycno", "Control")) %>%
  filter(timepoint == 5) %>%
  ggplot(aes(x = Treatment, y = gramsperhour, color = Treatment)) + 
  ggtitle("33 hours") +
  geom_point(col = "grey", size = 3) +
  scale_color_viridis(discrete = TRUE,
                      begin = 0.3,
                      end = 0.7,
                      option = "magma") +
  stat_summary(
    geom = "point",
    fun = "mean",
    size = 6,
    shape = 19
  ) +
  geom_errorbar(stat="summary", fun.data="mean_se", size = 1) +
  ylim(-0.06, 0.63) +
  labs(y = "", x = "Treatment") +
  theme_minimal(base_size = 15) +
  theme(axis.title.y = element_text(vjust = 3), axis.text.x=element_blank()) +
  theme(legend.position = "none") +
  annotate("text", x = 1.5, y = 0.62, label = "p = 0.12")

# FULL FIGURE 2
Figure2 <- ggarrange(feeding_plot, consumed_plot, consumed_plot2,
                     labels = c("A", "B", "C"),
                     ncol = 3,
                     nrow = 1,
                     widths = c(1, 0.3, 0.3),
                     common.legend = TRUE, legend = "top")
Figure2


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 2021 URCHIN MOVEMENT                                                         ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

str(trials2021_Q)

# Experimental structure:

# To detect indirect effects of Pycnopodia on purple urchin movement over short (i.e.:
# 1 hour) time scales and to determine if urchins will risk feeding depending on if
# they're well fed or starved, I ran an experiment testing various behaviors.
# Urchins were split into two groups, starved and fed, and held in those conditions
# (food withheld or given) for 7 weeks to precondition them for the experiment. 
# For the actual experiment I had two arenas (for convenience, identical) that I 
# would place an urchin into and watch for an hour. Like the previous experiment,
# one of the treatments was exposure to Pycnopodia water that was plumbed from a header
# tank into the center of the arena. There was also a no-Pycno control treatment.
# The other factors in the experiment were the starvation state of the particular urchin
# tested (fed, starved), and the presence or absence of a kelp cue in the middle of the
# tank next to the outflow from the header tank. This created a fully crossed design of 
# Pycno (present, absent), urchin (fed, starved), and kelp (present, absent). I would place
# the urchins in the tank and record their behavior every minute for an hour. The only
# behaviors I'm addressing here are movement (i.e.: how much time did they spend moving around
# versus sitting still), and how much time they spent 'interacting' with the cue 
# sources in the center of the tank (interacting, not interacting). For the purposes
# of the experiment, 'interacting' meant that the urchin had at least one tube foot 
# touching the kelp/kelp control, or the actual inflow from which the Pycno or control
# water was flowing from. 
#
# I also filmed all the trials with GoPro and used some software to extract xy coordinates
# set to a scale bar and calculate total distance moved by each urchin. 
#  
# The idea is that movement can be a proxy for 'escape response' and interaction with
# the signal (particularly in the treatments where kelp was present) can be a proxy
# for 'willingness to risk predation for food'. 

####################
#      ##    ###   #
#    ##  ##     #  #
#   #     #     #  # 
#   #     #    ##  #
#   #  #  #   ##   #
#    #  ##   ##    #
#     ### #  ####  #
#                  #
####################

# Primary question:

# Q2
# Did any of the treatments change amount of time urchins spent moving, foraging,
# or total distance moved?


# 5 minute increments
model_1_dat <- trials2021_Q
model_1_dat <- model_1_dat %>%
  select(trial, date, tank, urchinGroup, pycnoTreat, algalTreat, interaction, movement) %>%
  group_by(trial, date, tank, urchinGroup, pycnoTreat, algalTreat) %>%
  mutate(interacting = case_when(interaction == "ni" ~ 0,
                                 interaction == "i" ~ 1)) %>%
  mutate(movement = case_when(movement == "st" ~ 0,
                              movement == "ma" ~ 1,
                              movement == "mt" ~ 1,
                              movement == "mp" ~ 1)) 

model_1_dat_5 <- model_1_dat %>%
  mutate(minutes = c(rep(5, 5), rep(10, 5), rep(15, 5), rep(20, 5), rep(25, 5), rep(30, 5), rep(35, 5), rep(40, 5), rep(45, 5), rep(50, 5), rep(55, 5), rep(60, 5))) %>%
  group_by(trial, date, tank, urchinGroup, pycnoTreat, algalTreat, minutes) %>%
  summarise(foraging = mean(interacting), moving = mean(movement))


# FIGURE - time spent foraging
model_1_plotdat_5 <- model_1_dat_5 %>%
  unite(Treatment, pycnoTreat, algalTreat, sep = "/" ) %>%
  mutate(Treatment = case_when(Treatment == "pycno/control" ~ "Pycno",
                               Treatment == "control/nereo" ~ "Kelp",
                               Treatment == "control/control" ~ "Control",
                               Treatment == "pycno/nereo" ~ "Pycno/Kelp")) 

control_avg_forage <- 
  model_1_plotdat_5 %>%
  ungroup() %>%
  filter(Treatment == "Control") %>%
  summarize(avg = mean(foraging)) %>%
  pull(avg)

plot_forage <- model_1_plotdat_5 %>%
  ggplot(aes(y = foraging, x = Treatment, color = urchinGroup)) +
  geom_hline(aes(yintercept = control_avg_forage), color = "gray70", size = 0.8, linetype = "dashed") +
  geom_jitter(position = position_jitter(seed = 227, width = 0.2), size = 2, alpha = 0.20) +
  geom_errorbar(stat="summary", fun.data="mean_se", size = 1, width = 0.3, position = position_jitter(seed = 227, width = 0.2)) +scale_y_continuous(
    limits = c(-0.1, 1.1), expand = c(0.005, 0.005)) +
  stat_summary(fun = mean, geom = "point", size = 4, shape = 21, fill = "grey30", position = position_jitter(seed = 227, width = 0.2)) +
  scale_y_continuous(
    limits = c(-0.1, 1.1), expand = c(0.005, 0.005), position = "right") +
  scale_color_viridis(discrete = TRUE, begin = 0.3, end = 0.8, option = "H") +
  scale_fill_viridis(discrete = TRUE, begin = 0.3, end = 0.8, option = "H") +
  labs(color = "Urchin Group") +
  labs(y = "", x = NULL) +
  ggtitle("") +
  theme_bw() +
  theme(
    axis.title = element_text(size = 12),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    panel.grid = element_blank()
  ) 

# FIGURE - time spent moving 
model_1_plotdat_5 <- model_1_dat_5 %>%
  unite(Treatment, pycnoTreat, algalTreat, sep = "/" ) %>%
  mutate(Treatment = case_when(Treatment == "pycno/control" ~ "Pycno",
                               Treatment == "control/nereo" ~ "Nereo",
                               Treatment == "control/control" ~ "Control",
                               Treatment == "pycno/nereo" ~ "Pycno/Nereo"))

control_avg_move <- 
  model_1_plotdat_5 %>%
  ungroup() %>%
  filter(Treatment == "Control") %>%
  summarize(avg = mean(moving)) %>%
  pull(avg)

plot_move <- model_1_plotdat_5 %>%
  ggplot(aes(y = moving, x = Treatment, color = urchinGroup)) +
  geom_hline(aes(yintercept = control_avg_move), color = "gray70", size = 0.8, linetype = "dashed") +
  geom_jitter(position = position_jitter(seed = 227, width = 0.2), size = 2, alpha = 0.20) +
  geom_errorbar(stat="summary", fun.data="mean_se", size = 1, width = 0.3, position = position_jitter(seed = 227, width = 0.2)) +scale_y_continuous(
    limits = c(-0.1, 1.1), expand = c(0.005, 0.005)) +
  stat_summary(fun = mean, geom = "point", size = 4, shape = 21, fill = "grey30", position = position_jitter(seed = 227, width = 0.2)) +
  scale_x_discrete(labels=c("Pycno/Nereo" = "Pycno/Kelp", "Pycno" = "Pycno",
                            "Nereo" = "Kelp", "Control" = "Control")) +
  scale_color_viridis(discrete = TRUE, begin = 0.3, end = 0.8, option = "H") +
  scale_fill_viridis(discrete = TRUE, begin = 0.3, end = 0.8, option = "H") +
  labs(color = "Urchin Group") +
  labs(y = "Proportion Time", x = NULL) +
  ggtitle("") +
  theme_bw() +
  theme(
    axis.title = element_text(size = 12),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(size = 16, hjust = 0.5),
    panel.grid = element_blank()
  ) 


# FIGURE - total distance moved
dist_data <- trials2021_trackerDist_QAQC %>%
  unite(Treatment, Pyc, Alg, sep = "/") %>%
  mutate(Treatment = case_when(Treatment == "Pycno/No Algae" ~ "Pycno",
                               Treatment == "No Pycno/Algae" ~ "Kelp",
                               Treatment == "No Pycno/No Algae" ~ "Control",
                               Treatment == "Pycno/Algae" ~ "Pycno/Kelp")) 

plot_dist <- dist_data %>%
  ggplot(aes(y = distance, x = Treatment, color = Urchin)) +
  geom_jitter(position = position_jitter(seed = 227, width = 0.2), size = 2, alpha = 0.20) +
  geom_errorbar(stat="summary", fun.data="mean_se", size = 1, width = 0.3, position = position_jitter(seed = 227, width = 0.2)) +
  stat_summary(fun = mean, geom = "point", size = 4, shape = 21, fill = "grey30", position = position_jitter(seed = 227, width = 0.2)) +
  scale_color_viridis(discrete = TRUE, begin = 0.3, end = 0.8, option = "H") +
  labs(color = "Urchin Group") +
  labs(y = "Distance Moved (cm)", x = NULL) +
  ggtitle("") +
  theme_bw() +
  theme(
    axis.title = element_text(size = 12),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(size = 16, hjust = 0.5)
  ) 

# FIGURE - total kelp consumed
algalplot <- algal_consumption
algalplot$algalTreat <- "nereo"  
algalplot <- algalplot %>%
  add_row(urchinGroup = "starved", pycnoTreat = "pycno", eaten = 0, algalTreat = "control") %>%
  add_row(urchinGroup = "starved", pycnoTreat = "control", eaten = 0, algalTreat = "control") %>%
  add_row(urchinGroup = "fed", pycnoTreat = "pycno", eaten = 0, algalTreat = "control") %>%
  add_row(urchinGroup = "fed", pycnoTreat = "control", eaten = 0, algalTreat = "control") %>%
  add_row(urchinGroup = "fed", pycnoTreat = "pycno", eaten = 0, algalTreat = "nereo") %>%
  add_row(urchinGroup = "fed", pycnoTreat = "control", eaten = 0, algalTreat = "nereo") 

# 0.01 g per cm^2 from confetti values (21mm, 0.341 g per confetti)
algalplot <- algalplot %>%
  mutate(gramseaten = eaten * 0.01)
  
plot_eaten <- algalplot %>%
    unite(Treatment, pycnoTreat, algalTreat, sep = "/" ) %>%
    mutate(Treatment = case_when(Treatment == "pycno/control" ~ "Pycno",
                                 Treatment == "control/nereo" ~ "Kelp",
                                 Treatment == "control/control" ~ "Control",
                                 Treatment == "pycno/nereo" ~ "Pycno/Kelp")) 

plot_kelp <- plot_eaten %>%
  filter(Treatment %in% c("Kelp", "Pycno/Kelp")) %>%
  ggplot(aes(y = gramseaten, x = Treatment, color = urchinGroup)) +
  geom_jitter(position = position_jitter(seed = 227, width = 0.2), size = 2, alpha = 0.20) +
  geom_errorbar(stat="summary", fun.data="mean_se", size = 1, width = 0.3, position = position_jitter(seed = 227, width = 0.2)) +
  stat_summary(fun = mean, geom = "point", size = 4, shape = 21, fill = "grey30", position = position_jitter(seed = 227, width = 0.2)) +
  scale_color_viridis(discrete = TRUE, begin = 0.3, end = 0.8, option = "H") +
  labs(color = "Urchin Group") +
  labs(y = "Consumed (g)", x = NULL) +
  scale_y_continuous(position = "right") +
  ggtitle("") +
  theme_bw() +
  theme(
    axis.title = element_text(size = 12),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(size = 16, hjust = 0.5)
  ) 


# FULL FIGURE 3
ggpubr::ggarrange(plot_move, plot_forage, plot_dist, plot_kelp, 
                  labels = c("A (moving)", "B (foraging)", "C (total distance)", "D (kelp consumed)"), 
                  label.x = c(0.03, -0.09, -0.03, -0.18),
                  ncol = 2,
                  nrow = 2,
                  heights = c(1, 0.5),
                  common.legend = TRUE, legend = "top")
# best size: 880 x 660


# SUPPLEMENTAL FIGURE OF DIAMETER AND FEEDING 

diam_plot <- per_urchin_hourly %>%
  ggplot() +
  geom_point(aes(x = diameter, y = gramsperhour, color = pycno)) +
  geom_smooth(method = "lm", size = 2, aes(x = diameter, y = gramsperhour, color = pycno)) +
  scale_color_viridis(name = "Pycnopodia Cue", discrete = TRUE,
                      begin = 0.3,
                      end = 0.7,
                      option = "magma") +
  xlab("Diameter (mm)") +
  ylab("Kelp Consumed (g/hr)") +
  theme_bw(base_size = 15) +
  theme(axis.title.y = element_text(vjust = 3))
  
diam_plot


####
#<<<<<<<<<<<<<<<<<<<<<<<<<<END OF SCRIPT>>>>>>>>>>>>>>>>>>>>>>>>#


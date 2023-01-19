## Raw version of the behavioral data analysis for a flanker task ##
## The data contains sensitive data and cannot be provided ##


#### Packages ####
rm(list=ls(all=T))
library(dplyr)
library(ggplot2)
library(zoo)

#### Read data ####
source('data_import.R')

#### Data preparation ####
##### Exclusion criteria #####
data_cleaned <- data %>%
  filter(!is.na(key_resp_trial.rt)) %>% 
  group_by(id) %>%
  mutate(rt.thresh_low = 0.1) %>%
  mutate(rt.thresh_upper = (mean(key_resp_trial.rt) + 2.5*sd(key_resp_trial.rt))) %>%
  rowwise() %>% 
  filter(key_resp_trial.rt > rt.thresh_low & key_resp_trial.rt < rt.thresh_upper)

##### Visual inspection #####
trl_viewer <- data_cleaned %>% 
  group_by(id) %>% 
  summarise(first_trlNr = min(trials.thisN),
            last_trlNr = max(trials.thisN),
            total_trlNr = length(trials.thisN))
trl_viewer
range(trl_viewer$total_trlNr)

##### Check requirements #####
normality_check <- data_cleaned %>% 
  group_by(id) %>% 
  summarise(statistic_RT = shapiro.test(key_resp_trial.rt)$statistic, 
            p.value_RT = shapiro.test(key_resp_trial.rt)$p.value,
            statistic_trlN = shapiro.test(trials.thisN)$statistic,
            p.value_trlN = shapiro.test(trials.thisN)$p.value)

normality_check

##### Data visualization #####
# per participant:
raw_plot_fac <- ggplot(data_cleaned, aes(x = trials.thisN, 
                                         y = key_resp_trial.rt*1000)) +
  geom_point(shape = 1) +
  facet_wrap(~id) +
  theme(legend.position = "none",
        panel.background = element_rect(colour="black", fill="white"),
        strip.background = element_rect(colour="white", fill="white"),
        axis.title = element_text(size = 12),
        panel.spacing.x = unit(0,"line"), 
        panel.spacing.y = unit(0,"line")
  ) +
  scale_x_continuous(expand=c(0,0)) +
  geom_vline(xintercept=0) +
  xlab("Trialnummer") +
  ylab("Reaktionszeit in ms")

raw_plot_fac

# aggregated over all participants:
raw_plot_all <- ggplot(data_cleaned, aes(x = trials.thisN, 
                                         y = key_resp_trial.rt*1000)) +
  geom_point(shape = 1) +
  xlab("Trialnummer") +
  ylab("Reaktionszeit in ms") +
  scale_x_continuous(expand=c(0,0)) +
  geom_vline(xintercept=0) +
  theme_classic()

raw_plot_all

#### Analysis ####
##### First level analysis #####
corr_dat_pears <- data_cleaned %>%
  group_by(id) %>% 
  summarise(statistic_corr = cor.test(key_resp_trial.rt, trials.thisN, method = 'pearson')$statistic,
            p.value_corr = cor.test(key_resp_trial.rt, trials.thisN, method = 'pearson')$p.value,
            r_corr = cor.test(key_resp_trial.rt, trials.thisN, method = 'pearson')$estimate)

corr_dat_pears

##### Second level analysis #####
corr_dat_pears <- corr_dat_pears %>%
  arrange(id) %>% 
  rowwise %>% 
  mutate(r.z = 0.5*log((1 + r_corr) / (1 - r_corr)))
corr_dat_pears

t.test(corr_dat_pears$r.z, mu = 0)


##### Exploratory analysis #####
###### Errors ######
# per participant
err_dat.lv1 <- data_cleaned %>% 
  group_by(id) %>% 
  summarise(trlNr = trials.thisN,
            acumErr = cumsum(key_resp_trial.corr == 10))
err_plot.lv1 <- ggplot(err_dat.lv1, aes(x = trlNr, y = acumErr)) +
  geom_point(shape = 1, size = 0.2) +
  facet_wrap(~id) +
  theme(legend.position = "none",
        panel.background = element_rect(colour="black", fill="white"),
        strip.background = element_rect(colour="white", fill="white"),
        axis.title = element_text(size = 12),
        panel.spacing.x = unit(0,"line"), 
        panel.spacing.y = unit(0,"line")
  ) +
  scale_x_continuous(expand=c(0,0)) +
  geom_vline(xintercept=0) +
  xlab("Trialnummer") +
  ylab("Kumulierte Fehleranzahl")
err_plot.lv1

# aggregated over all participants
err_dat.lv2 <- err_dat.lv1 %>%
  group_by(trlNr) %>% 
  summarise(mean_acumErr = mean(acumErr))
err_plot.lv2 <- ggplot(err_dat.lv2, aes(x = trlNr, y = mean_acumErr)) +
  geom_point(shape = 1) +
  theme_classic() +
  xlab("Trialnummer") +
  ylab("Kumulierte Fehleranzahl")
err_plot.lv2

###### Sliding windows ######
## create new variables:
cov <- function(x){
  sd(x) / mean(x)
}

sliding_window <- function(variable, FUN){
  rollapply(variable,
            width=41,
            FUN=FUN,
            fill=NA,
            align='center',
            partial=FALSE)
}

sliding_data <- data_cleaned %>% 
  group_by(id) %>% 
  mutate(err=case_when(key_resp_trial.corr == 10 ~ 1,
                       TRUE ~ 0)) %>% 
  mutate(sliding_mean_rt=sliding_window(key_resp_trial.rt, mean),
         sliding_cov_rt=sliding_window(key_resp_trial.rt, cov),
         sliding_mean_er=sliding_window(err, mean),
         sliding_cov_er=sliding_window(err, cov)
  )

## Visualize:
# per participant:
sliding_plot <- function(yaxis, ytitle){
  p <- ggplot(data=sliding_data, aes(x=trials.thisN, y={{yaxis}}))
  p +
    geom_line() + 
    facet_wrap(~id) +
    theme_classic() +
    xlab("Trialnummer") +
    ylab(ytitle) +
    theme(legend.position = "none",
          panel.background = element_rect(colour="black", fill="white"),
          strip.background = element_rect(colour="white", fill="white"),
          axis.title = element_text(size = 12),
          panel.spacing.x = unit(0,"line"), 
          panel.spacing.y = unit(0,"line")
    )
}

sliding_plot_rt <- sliding_plot(sliding_mean_rt*1000, "Mittlere Reaktionszeit (ms)")
sliding_plot_rt_cov <- sliding_plot(sliding_cov_rt*1000, "CoV Reaktionszeit (ms)")
sliding_plot_error <- sliding_plot(sliding_mean_er, "Mittlere Fehleranzahl")
sliding_plot_error_cov <- sliding_plot(sliding_cov_er, "CoV Fehleranzahl")


sliding_plot_rt
sliding_plot_rt_cov
sliding_plot_error
sliding_plot_error_cov

# aggregated over all participants:
sliding_data_all <- sliding_data %>% 
  group_by(id) %>% 
  summarise(trlNr = trials.thisN,
            sliding_mean_rt,
            sliding_cov_rt,
            sliding_mean_er,
            sliding_cov_er
  )
sliding_data_all <- sliding_data_all %>%
  group_by(trlNr) %>% 
  summarise(rtm.sl = mean(sliding_mean_rt),
            rtcov.sl = mean(sliding_cov_rt),
            erm.sl = mean(sliding_mean_er),
            ercov.sl = mean(sliding_cov_er)
  )


sliding_plot_grouped <- function(yaxis, ytitle){
  p <- ggplot(sliding_data_all, aes(x=trlNr, y={{yaxis}}))
  p + geom_point(shape = 1) +
    geom_smooth(method = "lm", colour = "black", size = 0.2) +
    theme_classic() +
    xlab("Zeit (min)") +
    ylab(ytitle) +
    scale_x_continuous(breaks = c(0, 218, 436, 654, 872),
                       labels = c(0, 10, 20, 30, 40))
}

slid_plot_grouped_rtmean <- sliding_plot_grouped(rtm.sl, "Mittlere Reaktionszeit (ms)")
slid_plot_grouped_rtcov <- sliding_plot_grouped(rtcov.sl, "CoV der Reaktionszeit (ms)")
slid_plot_grouped_errmean <- sliding_plot_grouped(erm.sl, "Mittlere Fehleranzahl")

slid_plot_grouped_rtmean
slid_plot_grouped_rtcov
slid_plot_grouped_errmean

###### Exploratory slope testing #####
slope_parameters <- sliding_data %>% 
  group_by(id) %>% 
  do(mod1 = lm(sliding_mean_rt ~ trials.thisN, data = .),
     mod2 = lm(sliding_cov_rt ~ trials.thisN, data = .),
     mod3 = lm(sliding_mean_er ~ trials.thisN, data = .),
     mod4 = lm(sliding_cov_er ~ trials.thisN, data = .)
  ) %>% 
  summarise(slope_m.rt = summary(mod1)$coefficients[2,1],
            t_m.rt = summary(mod1)$coefficients[2,3],
            p_m.rt = summary(mod1)$coefficients[2,4],
            slope_cov.rt = summary(mod2)$coefficients[2,1],
            t_cov.rt = summary(mod2)$coefficients[2,3],
            p_cov.rt = summary(mod2)$coefficients[2,4],
            slope_m.err = summary(mod3)$coefficients[2,1],
            t_m.err = summary(mod3)$coefficients[2,3],
            p_m.err = summary(mod3)$coefficients[2,4],
            slope_cov.err = summary(mod4)$coefficients[2,1],
            t_cov.err = summary(mod4)$coefficients[2,3],
            p_cov.err = summary(mod4)$coefficients[2,4]
  )
slope_tests <- slope_parameters %>% 
  ungroup() %>% 
  summarise(m.rt_t = t.test(slope_m.rt, mu = 0)$statistic,
            m.rt_p = t.test(slope_m.rt, mu = 0)$p.value,
            cov.rt_t = t.test(slope_cov.rt, mu = 0)$statistic,
            cov.rt_p = t.test(slope_cov.rt, mu = 0)$p.value,
            m.err_t = t.test(slope_m.err, mu = 0)$statistic,
            m.err_p = t.test(slope_m.err, mu = 0)$p.value,
            cov.err_t = t.test(slope_cov.err, mu = 0)$statistic,
            cov.err_p = t.test(slope_cov.err, mu = 0)$p.value
  )
slope_tests

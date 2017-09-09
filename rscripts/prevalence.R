############################################################
#                                                          #
#        Generate prevalence data (run only once)          #
#                                                          #
############################################################
# Load packages
###############
library(boot)
library(dplyr)
library(tidyr)
library(tibble)

# NeP prevalence raw data
#########################
# Collate data manually extracted from 6 core studies
study <- c('Bouhassira et al., 2008', 'de Moraes Vieira et al., 2012',
           'Torrance et al., 2006', 'Toth et al., 2009',
           'Yawn et al., 2009', 'Harifi et al., 2013')
year <- c(2008, 2012, 2006, 2009, 2009, 2013)
pmid <- c(17888574, 22871508, 16618472, 19594844, 20849570, 23241023)
location <- c('France', 'Brazil', 'UK',
              'Canada', 'USA', 'Morocco')
instrument <- c('DN4-Interview','DN4-Interview', 'S-LANSS',
                'DN4-Interview', 'S-LANSS', 'DN4-Interview')
sample <- c(23713, 1597, 3002, 1207, 3575, 5328) # extracted from the papers
cases <- c(1631, 157, 241, 216, 315, 565) # extracted from the papers

# Put collated data into dataframe
prev_df <- data_frame(study = study, year = year, pmid = pmid,
                      location = location, n_cases = cases,
                      n_total = sample) %>%
    mutate(n_controls = n_total - n_cases) %>%
    mutate(prevalence = round(100 * (n_cases / n_total), 1)) %>%
    arrange(year)

# Simulate populations
######################
# Simulate populations (neup = neuropathic pain, no_neup = no neuropathic),
# and make a dataframe with a study reference column.
# Torrance
torrance <- sample(
    c(rep('neup', prev_df$n_cases[1]), rep('no_neup', prev_df$n_controls[1])),
    size = prev_df$n_total[1], replace = FALSE)
torrance <- data_frame(study_ID = rep('torrance', prev_df$n_total[1]),
                       status = torrance)
# Bouhassira
bouhassira <- sample(
    c(rep('neup', prev_df$n_cases[2]), rep('no_neup', prev_df$n_controls[2])),
    size = prev_df$n_total[2], replace = FALSE)
bouhassira <- data_frame(study_ID = rep('bouhassira', prev_df$n_total[2]),
                         status = bouhassira)
# Toth
toth <- sample(
    c(rep('neup', prev_df$n_cases[3]), rep('no_neup', prev_df$n_controls[3])),
    size = prev_df$n_total[3], replace = FALSE)
toth <- data_frame(study_ID = rep('toth', prev_df$n_total[3]),
                   status = toth)
# Yawn
yawn <- sample(
    c(rep('neup', prev_df$n_cases[4]), rep('no_neup', prev_df$n_controls[4])),
    size = prev_df$n_total[4], replace = FALSE)
yawn <- data_frame(study_ID = rep('yawn', prev_df$n_total[4]),
                   status = yawn)
# de Moraes Viera
de_moraes_vieira <- sample(
    c(rep('neup', prev_df$n_cases[5]), rep('no_neup', prev_df$n_controls[5])),
    size = prev_df$n_total[5], replace = FALSE)
de_moraes_vieira <- data_frame(study_ID = rep('de_moraes_vieira',
                                              prev_df$n_total[5]),
                               status = de_moraes_vieira)
# Harifi
harifi <- sample(
    c(rep('neup', prev_df$n_cases[6]), rep('no_neup', prev_df$n_controls[6])),
    size = prev_df$n_total[6], replace = FALSE)
harifi <- data_frame(study_ID = rep('harifi', prev_df$n_total[6]),
                     status = harifi)

# All
all <- torrance %>%
    bind_rows(bouhassira) %>%
    bind_rows(toth) %>%
    bind_rows(yawn) %>%
    bind_rows(de_moraes_vieira) %>%
    bind_rows(harifi) %>%
    sample_n(size = length(.$status))

# Bootstrap
###########
# Make 'statistic' function for 'boot'
bootstrp_prev <- function(data, i) {
    data_2 <- data$status[i]
    data_3 <- data_2[i]
    data_4 <- (length(data_3[data_3 == 'neup']) / length(data_3)) * 100
    data_4
}

# Bootstrap data
torrance_bstrp <- boot(data = torrance,
                       statistic = bootstrp_prev,
                       R = 10000,
                       stype = 'i')
torrance_95CI <- boot.ci(torrance_bstrp, type = 'basic')
torrance_summary <- data_frame(study = 'Torrance et al., 2006',
                               bstrp_prev = round(torrance_bstrp$t0, 3),
                               lowerCI = round(torrance_95CI$basic[4], 3),
                               upperCI = round(torrance_95CI$basic[5], 3),
                               min = round(min(torrance_bstrp$t), 3),
                               max = round(max(torrance_bstrp$t), 3))

bouhassira_bstrp <- boot(data = bouhassira,
                         statistic = bootstrp_prev,
                         R = 10000,
                         stype = 'i')
bouhassira_95CI <- boot.ci(bouhassira_bstrp, type = 'basic')
bouhassira_summary <- data_frame(study = 'Bouhassira et al., 2008',
                                 bstrp_prev = round(bouhassira_bstrp$t0, 3),
                                 lowerCI = round(bouhassira_95CI$basic[4], 3),
                                 upperCI = round(bouhassira_95CI$basic[5], 3),
                                 min = round(min(bouhassira_bstrp$t), 3),
                                 max = round(max(bouhassira_bstrp$t), 3))

toth_bstrp <- boot(data = toth,
                   statistic = bootstrp_prev,
                   R = 10000,
                   stype = 'i')
toth_95CI <- boot.ci(toth_bstrp, type = 'basic')
toth_summary <- data_frame(study = 'Toth et al., 2009',
                           bstrp_prev = round(toth_bstrp$t0, 3),
                           lowerCI = round(toth_95CI$basic[4], 3),
                           upperCI = round(toth_95CI$basic[5], 3),
                           min = round(min(toth_bstrp$t), 3),
                           max = round(max(toth_bstrp$t), 3))

yawn_bstrp <- boot(data = yawn,
                   statistic = bootstrp_prev,
                   R = 10000,
                   stype = 'i')
yawn_95CI <- boot.ci(yawn_bstrp, type = 'basic')
yawn_summary <- data_frame(study = 'Yawn et al., 2009',
                           bstrp_prev = round(yawn_bstrp$t0, 3),
                           lowerCI = round(yawn_95CI$basic[4], 3),
                           upperCI = round(yawn_95CI$basic[5], 3),
                           min = round(min(yawn_bstrp$t), 3),
                           max = round(max(yawn_bstrp$t), 3))

de_moraes_vieira_bstrp <- boot(data = de_moraes_vieira,
                               statistic = bootstrp_prev,
                               R = 10000,
                               stype = 'i')
de_moraes_vieira_95CI <- boot.ci(de_moraes_vieira_bstrp, type = 'basic')
de_moraes_vieira_summary <- data_frame(study = 'de Moraes Vieira et al., 2012',
                                       bstrp_prev =
                                           round(de_moraes_vieira_bstrp$t0, 3),
                                       lowerCI =
                                           round(de_moraes_vieira_95CI$basic[4], 3),
                                       upperCI =
                                           round(de_moraes_vieira_95CI$basic[5], 3),
                                       min = round(min(de_moraes_vieira_bstrp$t), 3),
                                       max = round(max(de_moraes_vieira_bstrp$t), 3))

harifi_bstrp <- boot(data = harifi,
                     statistic = bootstrp_prev,
                     R = 10000,
                     stype = 'i')
harifi_95CI <- boot.ci(harifi_bstrp, type = 'basic')
harifi_summary <- data_frame(study = 'Harifi et al., 2013',
                             bstrp_prev = round(harifi_bstrp$t0, 3),
                             lowerCI = round(harifi_95CI$basic[4], 3),
                             upperCI = round(harifi_95CI$basic[5], 3),
                             min = round(min(harifi_bstrp$t), 3),
                             max = round(max(harifi_bstrp$t), 3))

all_bstrp <- boot(data = all,
                  statistic = bootstrp_prev,
                  R = 10000,
                  stype = 'i')
all_95CI <- boot.ci(all_bstrp, type = 'basic')
all_summary <- data_frame(study = 'All combined',
                          year = NA,
                          pmid = NA,
                          location = NA,
                          n_cases = NA,
                          n_total = sum(23713, 1597, 3002, 1207, 3575, 5328),
                          n_controls = NA,
                          prevalence = NA,
                          bstrp_prev = round(all_bstrp$t0, 3),
                          lowerCI = round(all_95CI$basic[4], 3),
                          upperCI = round(all_95CI$basic[5], 3),
                          min = round(min(all_bstrp$t), 3),
                          max = round(max(all_bstrp$t), 3))

# Bind dataframes of bootstrapped data to extracted data
########################################################
# Bind summary objects (except 'all_summary')
prev_df2 <- torrance_summary %>%
    bind_rows(bouhassira_summary) %>%
    bind_rows(toth_summary) %>%
    bind_rows(yawn_summary) %>%
    bind_rows(de_moraes_vieira_summary) %>%
    bind_rows(harifi_summary)
# Bind extracted data ('prev_df') to summary objects
prev_df3 <- prev_df %>%
    bind_cols(prev_df2) %>%
    # Add 'all_summary' data
    bind_rows(all_summary) %>%
    # Convert 'study' column to an ordered factor
    mutate(study = factor(study, levels = c('Torrance et al., 2006',
                                            'Bouhassira et al., 2008',
                                            'Toth et al., 2009',
                                            'Yawn et al., 2009',
                                            'de Moraes Vieira et al., 2012',
                                            'Harifi et al., 2013',
                                            'All combined'),
                          labels = c('Torrance et al., 2006',
                                     'Bouhassira et al., 2008',
                                     'Toth et al., 2009',
                                     'Yawn et al., 2009',
                                     'de Moraes Vieira et al., 2012',
                                     'Harifi et al., 2013',
                                     'All combined'),
                          ordered = TRUE))

############################################################
#                                                          #
#             Generate prevalence plot objects             #
#                                                          #
############################################################
# Round numbers, and add tooltip and onclick
plot_data <- prev_df3 %>%
    mutate(bstrp_prev = round(bstrp_prev, 1),
           lowerCI = round(lowerCI, 1),
           upperCI = round(upperCI, 1),
           min = round(min, 1),
           max = round(max, 1),
           tooltip = paste0(
               '<b>', study, '</b> <br> <em>Sample size:</em> ', n_total, '<br> <em>Point prevalence (%):</em> ', bstrp_prev, '<br> <em>Bootstrap 95% CI (%):</em> ', lowerCI, ' to ', upperCI, '<br> <em>Bootstrap range (%):</em> ', min, ' to ', max))

# Set geom_rect and geom_segement x-values
xmin <- c(0.6, 1.6, 2.6, 3.6, 4.6, 5.6)
xmax <- c(1.4, 2.4, 3.4, 4.4, 5.4, 6.4)
x_segment <- c(1, 2, 3, 4, 5, 6)
bar_1 <- c(0.8, 1.8, 2.8, 3.8, 4.8, 5.8)
bar_2 <- c(1.2, 2.2, 3.2, 4.2, 5.2, 6.2)

# Set tooltip and hover css
tooltip_css <- 'background-color:#333333;padding:10px;border:1px solid #999999;border-radius:10px 25px;color:#FFFFFF;font-family:arial, helvetica, sans-serif'
hover_css <- "fill:#FFFFFF;opacity:1"

############################################################
#                                                          #
#                   Save as .RData files                   #
#                                                          #
############################################################
# Simulated datasets
save(list = c('torrance', 'bouhassira', 'toth', 'yawn',
              'de_moraes_vieira', 'harifi', 'all'),
     file = './data/prevalence_simulated.RData')

# Bootstrap output
save(list = c('torrance_bstrp', 'bouhassira_bstrp', 'toth_bstrp',
              'yawn_bstrp', 'de_moraes_vieira_bstrp',
              'harifi_bstrp', 'all_bstrp'),
     file = './data/prevalence_bootstrap.RData')

# Summary data
save(list = c('plot_data', 'xmin', 'xmax', 'x_segment',
              'bar_1', 'bar_2', 'tooltip_css', 'hover_css'),
     file = './data/prevalence_plot.RData')

# Clear environment
rm(list = ls())

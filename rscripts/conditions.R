############################################################
#                                                          #
#           Generate prevalence data for various           #
#         neuropathic conditions (run only once)           #
#                                                          #
############################################################
# Load packages
library(dplyr)
library(tidyr)
library(tibble)

# Make dataframe using data from Sadosky et al., 2008
data <- data_frame(id = 1:7,
                   condition = factor(c('Limb amputation',
                                        'Spinal cord injury',
                                        'HIV polyneuropathy',
                                        'Multiple sclerosis',
                                        'Post-herpetic neuralgia',
                                        'Diabetic polyneuropathy',
                                        'Stroke'),
                                      levels = c('Limb amputation',
                                                 'Spinal cord injury',
                                                 'HIV polyneuropathy',
                                                 'Multiple sclerosis',
                                                 'Post-herpetic neuralgia',
                                                 'Diabetic polyneuropathy',
                                                 'Stroke'),
                                      labels = c('Limb amputation',
                                                 'Spinal cord injury',
                                                 'HIV polyneuropathy*',
                                                 'Multiple sclerosis',
                                                 'Post-herpetic neuralgia**',
                                                 'Diabetic polyneuropathy*',
                                                 'Stroke'),
                                      ordered = TRUE),
                   lower_range = c(53, 10, 30, 23, 7, 11, 8),
                   upper_range = c(85, 80, 63, 58, 27, 26, 11),
                   source = c(rep('Sadosky et al., 2008', 7)))

# Add tooltip column
plot_data <- data %>%
    mutate(point_estimate = (upper_range - lower_range) / 2,
           tooltip = paste0('<b>', condition, '</b> <br><em>Prevalence range (%):</em> ', lower_range, ' to ', upper_range))

# Set tooltip and hover css
tooltip_css <- 'background-color:#333333;padding:10px;border:1px solid #999999;border-radius:10px 25px;color:#FFFFFF;font-family:arial, helvetica, sans-serif'
hover_css <- 'fill:#FFFFFF;opacity:1'

# Set palette
pal <- rep('#0072B2', 7)

############################################################
#                                                          #
#                   Save as .RData files                   #
#                                                          #
############################################################
# Summary data
save(list = c('plot_data', 'tooltip_css', 'hover_css', 'pal'),
     file = './data/conditions_plot.RData')

# Clear environment
rm(list = ls())

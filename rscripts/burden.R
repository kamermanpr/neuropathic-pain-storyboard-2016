############################################################
#                                                          #
#  Generate data for painful DPN figure (run only once)    #
#                                                          #
############################################################
# Load packages
library(dplyr)
library(tidyr)
library(tibble)

# Data extracted from:
# Guariguata L, Whiting DR, Hambleton I, Beagley J, Linnenkamp U, Shaw JE.
# Global estimates of diabetes prevalence for 2013 and projections for 2035.
# Diabetes Res. Clin. Pract. 2014;103:137–149.
# doi:10.1016/j.diabres.2013.11.002.

#Veves A, Backonja M, Malik RA. Painful diabetic neuropathy: epidemiology,
# natural history, early diagnosis, and treatment options. Pain Med. 2008;
# 9:660–674. doi:10.1111/j.1526-4637.2007.00347.x.

# Generate data
data_burden <- data_frame(year = c(1985, 1995, 2013, 2035),
                    # DM prevalence and DPN estimates (10% to 20% range)
                    # at each time-point ('year')
                    dm = c(75, 140, 382, 592),
                    dpn_upper = c(75 * 0.2, 140 * 0.2, 382 * 0.2, 592 * 0.2),
                    dpn_lower = c(75 * 0.1, 140 * 0.1, 382 * 0.1, 592 * 0.1),
                    # Tooltips
                    tooltip_dm = paste0('<em>Estimated DM burden: </em> ', dm, ' million people'),
                    tooltip_upper = paste0('<em>Upper estimate of DPN burden: </em> ', dpn_upper, ' million people'),
                    tooltip_lower = paste0('<em>Lower estimate of DPN burden: </em> ', dpn_lower, ' million people'))

# Polygon data
data_polygon <- data_frame(y = c(data_burden$dpn_upper,
                            rev(data_burden$dpn_lower)),
                           x = c(data_burden$year,
                            rev(data_burden$year)))

# Set tooltip and hover css
tooltip_css <- 'background-color:#333333;padding:10px;border:1px solid #999999;border-radius:10px 25px;color:#FFFFFF;font-family:arial, helvetica, sans-serif'

hover_css <- "fill:#FFFFFF;opacity:1"

############################################################
#                                                          #
#                   Save as .RData files                   #
#                                                          #
############################################################
save(list = c('data_burden', 'data_polygon', 'tooltip_css', 'hover_css'),
     file = './data/burden_plot.RData')

rm(list = ls())

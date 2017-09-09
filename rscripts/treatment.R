############################################################
#                                                          #
#            Generate data on treatment options            #
#           for neuropathic pain (run only once)           #
#                                                          #
############################################################
# Load packages
library(dplyr)
library(tibble)

# Generate data
## Data taken from the Table 1 of Finnerup et al 2015
## (http://dx.doi.org/10.1016/S1474-4422(14)70251-0)
nnt_data <- data_frame(drug = c('Tricyclic\nantidepressants',
                                '5HT and NA reuptake\ninhibitors',
                                'Gabapentin',
                                'Pregabalin'),
                       nnt = c(3.6, 6.4, 7.2, 7.7),
                       upperCI = c(4.4, 8.4, 9.1, 9.4),
                       lowerCI = c(3.0, 5.2, 5.9, 6.5),
                       participants = c(948, 2541, 3503, 5940),
                       # Add tooltip (first need to remove '\n' from drug names)
                       tooltip_drug = c('Tricyclic antidepressants',
                                '5HT and NA reuptake inhibitors',
                                'Gabapentin',
                                'Pregabalin'),
                       tooltip = paste0('<b>', tooltip_drug, '</b> <br> <em>Total trial participants:</em> ', participants, '<br> <em>NNT (95% CI):</em> ', nnt, ' (', lowerCI, ' to ', upperCI, ')'))

# Set tooltip and hover css
tooltip_css <- 'background-color:#333333;padding:10px;border:1px solid #999999;border-radius:10px 25px;color:#FFFFFF;font-family:arial, helvetica, sans-serif'
hover_css <- "fill:#FFFFFF;opacity:1"

############################################################
#                                                          #
#                   Save as .RData files                   #
#                                                          #
############################################################
save(list = c('nnt_data', 'tooltip_css', 'hover_css'),
     file = './data/treatment_plot.RData')

rm(list = ls())

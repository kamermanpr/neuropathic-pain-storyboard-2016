############################################################
#                                                          #
#          Generate polygon data (run only once)           #
#                                                          #
############################################################
# Load packages
library(readr)
library(dplyr)
library(tidyr)
library(tibble)
library(stringr)
library(geojsonio)
library(sp)
library(leaflet)

# Load data
access_data <- read_csv('./data/2015-neml-data.csv')

# Filtering objects
drug_class <- c('Anticonvulsant', 'SNRI', 'TCA', 'Opioid', 'Topical')
drug_name <- c('Gabapentin', 'Pregabalin', 'Duloxetine', 'Venlafaxine',
               'Amitriptyline', 'Clomipramine', 'Desipramine', 'Imipramine',
               'Nortriptyline', 'Morphine', 'Oxycodone', 'Tramadol',
               'Capsaicin', 'Lidocaine')
first_line <- c('Gabapentin', 'Pregabalin', 'Duloxetine', 'Venlafaxine',
                'Amitriptyline', 'Clomipramine', 'Desipramine', 'Imipramine',
                'Nortriptyline')
simplify_tca <- 'Amitriptyline|Clomipramine|Desipramine|Imipramine|Nortriptyline'
second_line <- c('Tramadol', 'Capsaicin', 'Lidocaine')
third_line <- c('Morphine', 'Oxycodone')

# Remove extraneous drugs/countries
access_data <- access_data  %>%
    # Select required columns
    select(Country, Class, Drug, Listed) %>%
    # Get rid of Sweden (OECD High Income)
    filter(Country != 'Sweden') %>%
    # Make lower case
    rename(country = Country,
           class = Class,
           drug = Drug,
           listed = Listed) %>%
    ## filter by drug name
    filter(drug %in% drug_name)

# Get country names from access_data df for expanding first_line df (see below)
countries_access <- access_data$country

# Extract first-line drugs per country
first_line <- access_data %>%
    filter(drug %in% first_line) %>%
    mutate(grade = rep('first_line', nrow(.))) %>%
    filter(listed == 'Yes') %>%
    # Simplify TCAs
    mutate(drug = str_replace(drug,
                              pattern = simplify_tca,
                              replacement = 'Tricyclic')) %>%
    select(country, grade, class, drug) %>%
    group_by(country, grade, class, drug) %>%
    summarise(n = n()) %>%
    # Flesh-out countries so that they all have all second-line drugs
    complete(country = unique(countries_access), drug) %>%
    unique(.) %>%
    # Convert NA to 0
    mutate(n = str_replace_na(n)) %>%
    mutate(n = as.numeric(str_replace(n,
                                      pattern = 'NA',
                                      replacement = '0'))) %>%
    # Recode all values > 0 as 1 (i.e., yes or no)
    mutate(n = ifelse(test = n > 0, yes = 1, no = 0)) %>%
    # Spread and flatten across drug classes
    spread(drug, n) %>%
    ungroup() %>%
    select(-class) %>%
    mutate(Tricyclic = str_replace_na(Tricyclic),
           Gabapentin = str_replace_na(Gabapentin),
           Pregabalin = str_replace_na(Pregabalin),
           Duloxetine = str_replace_na(Duloxetine),
           Venlafaxine = str_replace_na(Venlafaxine)) %>%
    mutate(Tricyclic = as.numeric(str_replace(Tricyclic,
                                              pattern = 'NA',
                                              replacement = '0')),
           Gabapentin = as.numeric(str_replace(Gabapentin,
                                               pattern = 'NA',
                                               replacement = '0')),
           Pregabalin = as.numeric(str_replace(Pregabalin,
                                               pattern = 'NA',
                                               replacement = '0')),
           Duloxetine = as.numeric(str_replace(Duloxetine,
                                               pattern = 'NA',
                                               replacement = '0')),
           Venlafaxine = as.numeric(str_replace(Venlafaxine,
                                                pattern = 'NA',
                                                replacement = '0'))) %>%
    # Calculate totals per drug per country
    # (used to reduce rows from 345 to 115 after expansion of '0' rows
    # caused by previous steps)
    group_by(grade, country) %>%
    summarise(tricyclic = sum(Tricyclic),
              gabapentin = sum(Gabapentin),
              pregabalin = sum(Pregabalin),
              duloxetine = sum(Duloxetine),
              venlafaxine = sum(Venlafaxine)) %>%
    # Calculate class totals
    mutate(class_tca = ifelse(tricyclic > 0,
                              yes = 1, no = 0),
           class_alpha2delta = ifelse(gabapentin + pregabalin > 0,
                                      yes = 1, no = 0),
           class_snri = ifelse(duloxetine + venlafaxine > 0,
                               yes = 1, no = 0)) %>%
    # Calculate classes per country
    mutate(classes_first = class_tca + class_alpha2delta + class_snri) %>%
    # Convert 0 and 1 to 'No' and 'Yes' for the drug columns (for popup)
    mutate(tricyclic = ifelse(tricyclic == 1,
                              yes = 'yes', no = 'no'),
              gabapentin = ifelse(gabapentin == 1,
                                  yes = 'yes', no = 'no'),
              pregabalin = ifelse(pregabalin == 1,
                                  yes = 'yes', no = 'no'),
              duloxetine = ifelse(duloxetine == 1,
                                  yes = 'yes', no = 'no'),
              venlafaxine = ifelse(venlafaxine == 1,
                                   yes = 'yes', no = 'no')) %>%
    # Rename for merging to SPDF
    rename(name = country) %>%
    # Reorder columns
    select(name, grade, tricyclic, gabapentin, pregabalin, duloxetine,
           venlafaxine, class_tca, class_alpha2delta,
           class_snri, classes_first) %>%
    # Get rid of strange attr that have cropped-up
    tbl_df()

# Extract second-line drugs per country
second_line <- access_data %>%
    filter(drug %in% second_line) %>%
    mutate(grade = rep('second_line', nrow(.))) %>%
    filter(listed == 'Yes') %>%
    select(country, grade, class, drug) %>%
    group_by(country, grade, class, drug) %>%
    summarise(n = n()) %>%
    # Flesh-out countries so that they all have all second-line drugs
    complete(country = countries_access, drug) %>%
    unique(.) %>%
    # Convert NA to 0
    mutate(n = str_replace_na(n)) %>%
    mutate(n = as.numeric(str_replace(n,
                                      pattern = 'NA',
                                      replacement = '0'))) %>%
    # Recode all values > 0 as 1 (i.e., yes or no)
    mutate(n = ifelse(test = n > 0, yes = 1, no = 0)) %>%
    # Spread and flatten across drug classes
    spread(drug, n) %>%
    ungroup() %>%
    select(-class) %>%
    mutate(Lidocaine = str_replace_na(Lidocaine),
           Tramadol = str_replace_na(Tramadol)) %>%
    mutate(Lidocaine = as.numeric(str_replace(Lidocaine,
                                              pattern = 'NA',
                                              replacement = '0')),
           Tramadol = as.numeric(str_replace(Tramadol,
                                             pattern = 'NA',
                                             replacement = '0'))) %>%
    # Calculate totals per drug per country
    group_by(grade, country) %>%
    summarise(lidocaine = sum(Lidocaine),
              tramadol = sum(Tramadol)) %>%
    # Calculate class totals
    mutate(class_opioid2 = ifelse(tramadol > 0,
                                 yes = 1, no = 0),
           class_topical = ifelse(lidocaine > 0,
                                  yes = 1, no = 0)) %>%
    # Calculate classes per country
    mutate(classes_second = class_opioid2 + class_topical) %>%
    # Convert 0 and 1 to 'No' and 'Yes' for the drug columns (for popup)
    mutate(tramadol = ifelse(tramadol == 1,
                             yes = 'yes', no = 'no'),
           lidocaine = ifelse(lidocaine == 1,
                              yes = 'yes', no = 'no')) %>%
    # Make sure the is a zero for the classes_total (for plotting)
    complete(classes_second = c(0, 1, 2)) %>%
    # Rename for merging to SPDF
    rename(name = country) %>%
    # Reorder columns
    select(name, grade, tramadol, lidocaine, class_opioid2,
           class_topical, classes_second) %>%
    # Get rid of strange attr that have cropped-up
    tbl_df()

# Extract third-line drugs per country
third_line <- access_data %>%
    filter(drug %in% third_line) %>%
    mutate(grade = rep('third_line', nrow(.))) %>%
    filter(listed == 'Yes') %>%
    select(country, grade, class, drug) %>%
    group_by(country, grade, class, drug) %>%
    summarise(n = n()) %>%
    # Flesh-out countries so that they all have all second-line drugs
    complete(country = countries_access, drug) %>%
    unique(.) %>%
    # Convert NA to 0
    mutate(n = str_replace_na(n)) %>%
    mutate(n = as.numeric(str_replace(n,
                                      pattern = 'NA',
                                      replacement = '0'))) %>%
    # Recode all values > 0 as 1 (i.e., yes or no)
    mutate(n = ifelse(test = n > 0, yes = 1, no = 0)) %>%
    # Spread and flatten across drug classes
    spread(drug, n) %>%
    ungroup() %>%
    select(-class) %>%
    mutate(Morphine = str_replace_na(Morphine), # None had Botox
           Oxycodone = str_replace_na(Oxycodone)) %>%
    mutate(Morphine = as.numeric(str_replace(Morphine,
                                             pattern = 'NA',
                                             replacement = '0')),
           Oxycodone = as.numeric(str_replace(Oxycodone,
                                              pattern = 'NA',
                                              replacement = '0'))) %>%
    # Calculate totals per drug per country
    group_by(grade, country) %>%
    summarise(morphine = sum(Morphine),
              oxycodone = sum(Oxycodone)) %>%
    # Calculate class totals
    mutate(classes_third = ifelse(morphine > 0 | oxycodone > 0,
                                   yes = 1, no = 0)) %>%
    # Convert 0 and 1 to 'No' and 'Yes' for the drug columns (for popup)
    mutate(morphine = ifelse(morphine == 1,
                             yes = 'yes', no = 'no'),
           oxycodone = ifelse(oxycodone == 1,
                              yes = 'yes', no = 'no')) %>%
    # Make sure the is a zero for the classes_total (for plotting)
    complete(classes_third = c(0, 1, 2)) %>%
    # Get rid of strange NA row that gets added when using 'complete'
    filter(!is.na(country)) %>%
    # Rename for merging to SPDF
    rename(name = country) %>%
    # Reorder columns
    select(name, grade, morphine, oxycodone, classes_third) %>%
    # Get rid of strange attr that have cropped-up
    tbl_df()

############################################################
#                                                          #
#                  Make geospacial object                  #
#                                                          #
############################################################
# Load geojson
geo_data <- geojson_read('./data/admin0_countries_50m.geojson',
                         what = 'sp')

# Simplify SPDF to avoid NAs
## Filter geo_data by countries in first_line df
geo_data <- geo_data[geo_data$name %in% countries_access,
                      c('name', 'income_grp', 'gdp_md_est')]
## Filter *_line dfs by countries in geo_data
countries_geo <- geo_data$name
first_line <- first_line[first_line$name %in% countries_geo, ]
second_line <- second_line[second_line$name %in% countries_geo, ]
third_line <- third_line[third_line$name %in% countries_geo, ]

## Factor 'classes_*'
first_line$factor_first <- factor(first_line$classes_first,
                                  levels = c('0', '1', '2', '3'),
                                  ordered = TRUE)
second_line$factor_second <- factor(second_line$classes_second,
                                  levels = c('0', '1', '2'),
                                  ordered = TRUE)
third_line$factor_third <- factor(third_line$classes_third,
                                  levels = c('0', '1', '2'),
                                  ordered = TRUE)

## Clean-up 'income grp'
imf <- data_frame(name = geo_data$name, imf = geo_data$income_grp) %>%
    mutate(imf = str_replace(imf,
                             pattern = '1. High income: OECD',
                             replacement = 'High income')) %>%
    mutate(imf = str_replace(imf,
                             pattern = '2. High income: nonOECD',
                             replacement = 'High income')) %>%
    mutate(imf = str_replace(imf,
                             pattern = '3. Upper middle income',
                             replacement = 'Upper-middle income')) %>%
    mutate(imf = str_replace(imf,
                             pattern = '4. Lower middle income',
                             replacement = 'Lower-middle income')) %>%
    mutate(imf = str_replace(imf,
                             pattern = '5. Low income',
                             replacement = 'Low income')) %>%
    mutate(imf = factor(imf,
                        levels = c('Low income', 'Lower-middle income',
                                   'Upper-middle income', 'High income'),
                        ordered = TRUE))

# Merge SPDF with *_line dfs
geo_data <- merge(x = geo_data, y = first_line,
                   by.x = 'name', by.y = 'name') %>%
    merge(x = ., y = second_line,
          by.x = 'name', by.y = 'name') %>%
    merge(x = ., y = third_line,
          by.x = 'name', by.y = 'name') %>%
    merge(x = ., y = imf,
          by.x = 'name', by.y = 'name')

# Remove 'income_grp' column
geo_data@data <- geo_data@data[-2]

# Palette (from Color Brewer single hue sequential palette)
pal_first <- colorFactor(c('#feedde','#fdbe85','#fd8d3c','#d94701'),
                         geo_data$factors_first, ordered = TRUE)
pal_second <- colorFactor(c('#efedf5','#bcbddc','#756bb1'),
                          geo_data$factor_second, ordered = TRUE)
pal_third <- colorFactor(c('#e5f5e0','#a1d99b','#31a354'),
                         geo_data$factor_third, ordered = TRUE)
pal_income <- colorFactor(c('#eff3ff','#bdd7e7','#6baed6','#2171b5'),
                          geo_data$imf, ordered = TRUE)
# Popups
popup_first <- paste0('<b>Country: </b>', geo_data$name,
                      ' (', geo_data$imf, ')',
                      '<br><b>No. 1st-line classes on NEML: </b>',
                      geo_data$classes_first,
                      '<br><em>TCA (any): </em>',
                      geo_data$tricyclic,
                      '<br><em>&alpha;2&delta;-ligand (gabapentin): </em>',
                      geo_data$gabapentin,
                      '<br><em>&alpha;2&delta;-ligand (pregabalin): </em>',
                      geo_data$pregabalin,
                      '<br><em>SNRI (duloxetine): </em>',
                      geo_data$duloxetine,
                      '<br><em>SNRI (venlafaxine): </em>',
                      geo_data$venlafaxine)

popup_second <- paste0('<b>Country: </b>', geo_data$name,
                       ' (', geo_data$imf, ')',
                       '<br><b>No. 2nd-line classes on NEML: </b>',
                       geo_data$classes_second,
                       '<br><em>Weak Opioid (Tramadol): </em>',
                       geo_data$tramadol,
                       '<br><em>Topical agent (5% lidocaine): </em>',
                       geo_data$lidocaine,
                       '<br><em>Topical agent (8% capsaicin): </em> no')

popup_third <- paste0('<b>Country: </b>', geo_data$name,
                      ' (', geo_data$imf, ')',
                      '<br><b>No. 3rd-line classes on NEML: </b>',
                      geo_data$classes_third,
                      '<br><em>Strong opioid (morphine): </em>',
                      geo_data$morphine,
                      '<br><em>Strong opioid (oxycodone): </em>',
                      geo_data$oxycodone,
                      '<br><em>Other (Botulinium toxin A): </em> no')

############################################################
#                                                          #
#                   Save as .RData files                   #
#                                                          #
############################################################
save(list = c('geo_data', 'popup_first', 'popup_second', 'popup_third',
              'pal_first', 'pal_second', 'pal_third', 'pal_income'),
     file = './data/access_plot.RData')

rm(list = ls())

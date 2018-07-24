# Data from http://ceo.gencat.cat/ca/inici
# Specific data set http://ceo.gencat.cat/ca/barometre/detall/index.html?id=6668
# Downloaded on 2018-05-17
# Raw survey questions at http://upceo.ceo.gencat.cat/wsceop/6668/Q%C3%BCestionari%20-885.pdf

# Libraries
library(tidyverse)

# Read data
df <- read_delim('Microdades anonimitzades cine -901.csv', delim = ';')

# Create some cleaned up variables
df <- df %>%
  mutate(weight = as.numeric(as.character(gsub(',', '.', PONDERA, fixed = TRUE))),
         sex = ifelse(SEXE == 1, 'M', 
                      ifelse(SEXE == 2, 'F', NA)),
         age = EDAT,
         birth = ifelse(C100 == 1, 'Catalonia',
                        ifelse(C100 == 2, 'Rest of Spain',
                               ifelse(C100 == 3, 'Europe',
                                      ifelse(C100 == 4, 'Rest of World',
                                             NA)))),
         birth_father = ifelse(C110 == 1, 'Catalonia',
                               ifelse(C110 == 2, 'Rest of Spain',
                                      ifelse(C110 == 3, 'Europe',
                                             ifelse(C110 == 4, 'Rest of World',
                                                    NA)))),
         birth_mother = ifelse(C120 == 1, 'Catalonia',
                               ifelse(C120 == 2, 'Rest of Spain',
                                      ifelse(C120 == 3, 'Europe',
                                             ifelse(C120 == 4, 'Rest of World',
                                                    NA)))),
         birth_grandparents = ifelse(C130 == 1, 0,
                                     ifelse(C130 == 2, 1,
                                            ifelse(C130 == 3, 2,
                                                   ifelse(C130 == 4, 3,
                                                          ifelse(C130 == 5, 4,
                                                                 NA))))),
         
         language = ifelse(C704 == 1, 'Catalan',
                           ifelse(C704 == 2, 'Spanish',
                                  ifelse(C704 == 3, 'Both',
                                         ifelse(C704 == 4, 'Aranes',
                                                'Other')))),
         language_first = ifelse(C705 == 1, 'Catalan',
                                 ifelse(C705 == 2, 'Spanish',
                                        ifelse(C705 == 3, 'Both',
                                               ifelse(C705 == 4, 'Aranes',
                                                      'Other')))),
         language_habitual = ifelse(C706 == 1, 'Catalan',
                                    ifelse(C706 == 2, 'Spanish',
                                           ifelse(C706 == 3, 'Both',
                                                  ifelse(C706 == 4, 'Aranes',
                                                         'Other')))),
         independence = ifelse(P31 == 1, 'Yes',
                               ifelse(P31 == 2, 'No',
                                      'Not sure / no answer')),
         voted21d = ifelse(P38A %in% c(1,2,3,4), 'No',
                           ifelse(P38A == 5, 'Yes',
                                  NA)),
         religion = ifelse(C202 %in% 1:3, 'Yes',
                           ifelse(C202 == 4, 'No',
                                  NA)),
         feel = ifelse(C700 == 1, 'Only Spanish',
                       ifelse(C700 == 2, 'More Spanish than Catalan',
                              ifelse(C700 == 3, 'Equally Spanish and Catalan',
                                     ifelse(C700 == 4, 'More Catalan than Spanish',
                                            ifelse(C700 == 5, 'Only Catalan',
                                                   NA))))))
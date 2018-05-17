# Data from http://ceo.gencat.cat/ca/inici
# Specific data set http://ceo.gencat.cat/ca/barometre/detall/index.html?id=6668
# Downloaded on 2018-05-17
# Raw survey questions at http://upceo.ceo.gencat.cat/wsceop/6668/Q%C3%BCestionari%20-885.pdf

# Libraries
library(tidyverse)

# Read data
df <- read_delim('Microdades anonimitzades cine -885.csv', delim = ';')

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

# Get income
income_dict <- 
  data_frame(C900 = 
               c(1:15, 98,99),
             income = c('0',
                        '<300',
                        '301-600',
                        '601-900',
                        '901-1000',
                        '1001-1200',
                        '1201-1800',
                        '1801-2000',
                        '2001-2400',
                        '2401-3000',
                        '3001-4000',
                        '4001-4500',
                        '4501-5000',
                        '5001-6000',
                        '>6000',
                        NA,
                        NA))
df <- df %>% left_join(income_dict, by = 'C900')
df <- df %>%
  mutate(income = factor(income,
                         levels = 
                           c('0',
                         '<300',
                         '301-600',
                         '601-900',
                         '901-1000',
                         '1001-1200',
                         '1201-1800',
                         '1801-2000',
                         '2001-2400',
                         '2401-3000',
                         '3001-4000',
                         '4001-4500',
                         '4501-5000',
                         '5001-6000',
                         '>6000')))

# Get elections info
elections_dict <- data_frame(
  P37 = c(1,3,4,6,10,12,20,21,22, 80, 93, 94, 96, 97, 98, 99),
  party = c('PPC', 'ERC', 'PSC', 'C\'s', 'CUP', 'Podemos',
            'PDeCAT', 'Junts per Catalunya',
            'Cat en Com Podem', 'Other', 'None', 'None', 'None', 'None', 'Does not know', 'No answer')
)
df <- df %>% left_join(elections_dict, by = 'P37')

# Get education level
education_dict <- data_frame(
  C500 = c(1:11, 80, 98, 99),
  education = c('1. Primary school',
                '1. Primary school',
                '1. Primary school',
                '2. Middle/high school',
                '2. Middle/high school',
                '3. Technical school',
                '3. Technical school',
                '4. University',
                '4. University',
                '5. Graduate school',
                '5. Graduate school',
                NA,NA,NA)
)
df <- left_join(df, education_dict)
df <- df %>% mutate(education = gsub(' ', '\n', education))



# Keep only relevant variables
df <- df %>%
  dplyr::select(weight,
                sex,
                age, 
                birth,
                birth_father,
                birth_mother,
                birth_grandparents,
                party,
                income,
                language,
                language_first,
                language_habitual,
                independence,
                voted21d,
                religion,
                feel,
                education)

# Get a variable for both parents born in Catalonia, etc.
df$birth_parents <- 
  ifelse(df$birth_father == 'Catalonia' &
           df$birth_mother == 'Catalonia',
         'Both from Catalonia',
         ifelse(df$birth_father == 'Catalonia' |
                  df$birth_mother == 'Catalonia',
                'One from Catalonia',
                'None from Catalonia'))
df$independence <- factor(df$independence,
                          levels = c('Yes',
                                     'No',
                                     'Not sure / no answer'))

df$language <- factor(df$language,
                      levels = c('Catalan',
                                 'Spanish',
                                 'Both',
                                 'Other'))
df$language_first <- factor(df$language_first,
                      levels = c('Catalan',
                                 'Spanish',
                                 'Both',
                                 'Other'))
df$language_habitual <- factor(df$language_habitual,
                      levels = c('Catalan',
                                 'Spanish',
                                 'Both',
                                 'Other'))

df$feel <- gsub(' ', '\n', df$feel)
df$feel <- factor(df$feel,
                  levels = c('Only\nCatalan',
                             'More\nCatalan\nthan\nSpanish',
                             'Equally\nSpanish\nand\nCatalan',
                             'More\nSpanish\nthan\nCatalan',
                             'Only\nSpanish'))

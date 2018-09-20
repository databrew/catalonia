library(tidyverse)
library(foreign)
library(databrew)

df <- read.spss('2014_Microdades_anonimitzades_fusio_cine_pres.sav')
df <- data.frame(df)
# Create date
df$date <- 
  as.Date(paste0(ifelse(df$ANY < 2000,
                        df$ANY + 2000,
                        df$ANY), '-', df$MES, '-01'))

# Functions
mround <- function(x,base){ 
  base*round(x/base) 
} 

round_percent <- function(x) { 
  x <- x/sum(x)*100  # Standardize result
  res <- floor(x)    # Find integer bits
  rsum <- sum(res)   # Find out how much we are missing
  if(rsum<100) { 
    # Distribute points based on remainders and a random tie breaker
    o <- order(x%%1, sample(length(x)), decreasing=TRUE) 
    res[o[1:(100-rsum)]] <- res[o[1:(100-rsum)]]+1
  } 
  res 
}


# Age
# Education axis
# Left-right axis
# Division opposition
# View from outside

# Age curve
x <- df %>%
  filter(date >= '2018-01-01') %>%
  mutate(independence = P31) %>%
  mutate(independence = ifelse(independence %in% c('No ho sap', 
                                                   'No contesta'),
                               'NS/NC', as.character(independence))) %>%
  mutate(age = EDAT) %>%
  group_by(age, #age = mround(age, 5), 
           independence) %>%
  tally %>%
  ungroup %>%
  group_by(age) %>%
  mutate(p = n / sum(n) * 100) %>%
  ungroup %>%
  mutate(independence = 
           ifelse(independence == 'Sí',
                  'A favor de la independencia',
                  ifelse(independence == 'No',
                         'En contra de la independencia',
                         independence)))

cols <- c('#3492C7',
          '#FF5733',
          '#E5c388')
# cols <- rev(cols)

ggplot(data = x,
       aes(x = age,
           y = p,
           color = independence,
           group = independence)) +
  geom_point(alpha = 0.5,
             aes(size = n)) +
  geom_line(stat = 'smooth', method = 'auto', 
            # formula = y ~ poly(x, 1),
            size = 2, alpha = 0.6) +
  theme_databrew() +
  theme(legend.position = 'right') +
  xlim(20, 90) +
  scale_color_manual(name = 'Opinión sobre\nindependencia',
                     values = cols) +
  labs(x = 'Edad',
       y = 'Porcentaje',
       title = 'Asociación de edad y opinión sobre independencia',
       subtitle = 'Los jóvenes están a favor de la independencia; los mayores en contra.',
       caption = 'Datos: combinación de las 2 encuestas CEO de 2018.\n3,000 residentes de Cataluña con ciudadanía de España, edad 18+.') +
  ylim(0, 76) +
  geom_hline(yintercept = 50,
             alpha = 0.6) +
  scale_size(name = 'Tamaño de\nmuestra', breaks = c(5,15,40), labels = waiver(),
             limits = NULL, range = c(0.3,3), trans = "identity", guide = "legend")


education_dict <-data_frame(
  C500 = c(1:15),
  education = gsub(' ', '\n',c('0. Ninguna',
                               '1. Primaria',
                               '1. Primaria',
                               '2. Secundaria',
                               '2. Secundaria',
                               '3. Técnica',
                               '3. Técnica',
                               '4. Universidad\n(lic, grado...)',
                               '4. Universidad\n(lic, grado...)',
                               '5. Postgrado\n(master, doctorado...)',
                               '5. Postgrado\n(master, doctorado...)',
                               NA,NA,NA,NA)))

# Education curve
x <- df %>%
  filter(date >= '2017-09-01') %>%
  mutate(independence = P31) %>%
  mutate(independence = ifelse(independence %in% c('No ho sap', 
                                                   'No contesta'),
                               'NS/NC', as.character(independence))) %>%
  mutate(age = EDAT) %>%
  mutate(C500 = as.numeric(C500)) %>%
  left_join(education_dict) %>%
  filter(!is.na(education)) %>%
  filter(!education %in% c('95',
                           'No ho sap',
                           'No contesta')) %>%
  group_by(education, 
           independence) %>%
  tally %>%
  ungroup %>%
  group_by(education) %>%
  mutate(p = n / sum(n) * 100) %>%
  ungroup %>%
  mutate(independence = 
           ifelse(independence == 'Sí',
                  'A favor de la independencia',
                  ifelse(independence == 'No',
                         'En contra de la independencia',
                         independence)))
cols <- c('#3492C7',
          '#FF5733',
          '#E5c388')

ggplot(data = x,
       aes(x = education,
           y = p,
           color = independence,
           group = independence)) +
  geom_point(alpha = 0.5,
             aes(size = n)) +
  geom_line(stat = 'smooth', method = 'auto', 
            # formula = y ~ poly(x, 1),
            size = 2, alpha = 0.6) +
  theme_databrew() +
  theme(legend.position = 'right') +
  scale_color_manual(name = 'Opinión sobre\nindependencia',
                     values = cols) +
  labs(x = 'Formación',
       y = 'Porcentaje',
       caption = 'Datos: combinación de las 2 encuestas CEO de 2018.\n3,000 residentes de Cataluña con ciudadanía de España, edad 18+.',
       title = 'Asociación de educación y opinión sobre independencia',
       subtitle = 'Cuanto más formación académica, más a favor de la independencia.') +
  # ylim(0, 76) +
  geom_hline(yintercept = 50,
             alpha = 0.6) +
  scale_size(name = 'Tamaño de\nmuestra', 
             # breaks = c(10,30,50), 
             labels = waiver(),
             limits = NULL, range = c(1,5), trans = "identity", guide = "legend") +
  theme(axis.text.x = element_text(angle = 0))



# Left right curve
x <- df %>% 
  filter(date >= '2018-01-01')  %>%
  mutate(axis = P25)
x$axis <- as.character(x$axis)
x$axis <- ifelse(x$axis == 'Extrema esquerra', '0',
                 ifelse(x$axis == 'Extrema dreta', '10',
                        ifelse(x$axis %in% c('No ho sap', 'No contesta'), NA,
                               x$axis)))
x$axis2 <- as.numeric(as.character(x$axis))
x$axis2 <- factor(x$axis2,
                  levels = as.character(sort(unique(x$axis2))))
x$axis <- x$axis2

x <- x %>%
  mutate(independence = P31) %>%
  mutate(independence = ifelse(independence %in% c('No ho sap', 
                                                   'No contesta'),
                               'NS/NC', as.character(independence))) %>%
  group_by(axis,
           independence) %>%
  tally %>%
  ungroup %>%
  group_by(axis) %>%
  mutate(p = n / sum(n) * 100) %>%
  ungroup %>%
  mutate(independence = 
           ifelse(independence == 'Sí',
                  'A favor de la independencia',
                  ifelse(independence == 'No',
                         'En contra de la independencia',
                         independence)))
cols <- c('#3492C7',
          '#FF5733',
          '#E5c388')
# cols <- rev(cols)
ggplot(data = x %>%
         filter(!is.na(axis)),
       aes(x = axis,
           y = p,
           color = independence,
           group = independence)) +
  geom_point(alpha = 0.5,
             aes(size = n)) +
  geom_line(stat = 'smooth', method = 'auto', 
            # formula = y ~ poly(x, 1),
            size = 2, alpha = 0.6) +
  theme_databrew() +
  theme(legend.position = 'right') +
  scale_color_manual(name = 'Opinión sobre\nindependencia',
                     values = cols) +
  labs(x = 'Ideología política',
       y = 'Porcentaje',
       caption = 'Datos: combinación de las 2 encuestas CEO de 2018.\n3,000 residentes de Cataluña con ciudadanía de España, edad 18+.',
       title = 'Asociación de ideología y opinión sobre la independencia',
       subtitle = 'Cuanto más a la derecha, más en contra de la independencia.') +
  geom_hline(yintercept = 50,
             alpha = 0.6) +
  scale_size(name = 'Tamaño de muestra', breaks = c(10,30,50), labels = waiver(),
             limits = NULL, range = c(1,5), trans = "identity", guide = "legend") +
  scale_x_discrete(breaks = as.character(c(1, 3, 5, 7, 9)), labels=c('Extrema\nizquierda', 'Izquierda', 'Centro', 'Derecha', 'Extrema\nderecha')) 


# Curve 4: Division of opposition re: autonomy
x <- df %>% 
  filter(date >= '2018-01-01') %>%
  mutate(autonomy = P29) %>%
  mutate(autonomy = 
           ifelse(autonomy == 'Massa autonomia', 'Demasiado autonomía',
                  ifelse(autonomy == "Un nivell suficient d'autonomia",
                         'Suficiente autonomía',
                         ifelse(autonomy == 'Un nivell insuficient d\'autonomia', 'Demasiado poca autonomía', NA))))
x$autonomy <- factor(x$autonomy,
                     levels = c('Demasiado poca autonomía',
                                'Suficiente autonomía',
                                'Demasiado autonomía'))


x <- x %>%
  mutate(independence = P31) %>%
  mutate(independence = ifelse(independence %in% c('No ho sap', 
                                                   'No contesta'),
                               'NS/NC', as.character(independence))) %>%
  filter(!is.na(autonomy),
         !is.na(independence)) %>%
  group_by(autonomy,
           independence) %>%
  tally %>%
  ungroup %>%
  group_by(independence) %>%
  mutate(p = round_percent(n)) %>%
  ungroup %>%
  mutate(independence = 
           ifelse(independence == 'Sí',
                  'A favor de la independencia',
                  ifelse(independence == 'No',
                         'En contra de la independencia',
                         independence)))

cols <- c('#3492C7',
          '#E5c388',
          '#FF5733')

x <- x %>% filter(independence %in%
                    c('En contra de la independencia',
                      'A favor de la independencia'))

library(waffle)
y <- x %>% filter(independence == 'A favor de la independencia')
vec <- round(y$p)
names(vec) <- y$autonomy
gg <- waffle(parts = vec,
             colors = cols) +
  theme_databrew() +
  theme(legend.position = 'right')
gg <- gg + scale_x_continuous(expand = c(0, 0))
gg <- gg + scale_y_continuous(expand = c(0, 0))
gg <- gg + coord_equal()
gg <- gg + theme(panel.grid = element_blank())
gg <- gg + theme(panel.border = element_blank())
gg <- gg + theme(panel.background = element_blank())
gg <- gg + theme(panel.spacing = unit(0, "null"))
gg <- gg + theme(axis.text = element_blank())
gg <- gg + theme(axis.title.x = element_text(size = 10))
gg <- gg + theme(axis.ticks = element_blank())
gg <- gg + theme(axis.line = element_blank())
gg <- gg + theme(axis.ticks.length = unit(0, "null"))
gg <- gg + theme(plot.title = element_text(size = 18))
gg <- gg + theme(plot.background = element_blank())
gg <- gg + theme(panel.spacing = unit(c(0, 0, 0, 0), "null"))
gg <- gg + guides(fill=guide_legend(ncol=1)) + theme(legend.text = element_text(size = 17))
gg +
  labs(title = 'Entre los que están a favor de la independencia')


y <- x %>% filter(independence == 'En contra de la independencia')
vec <- round(y$p)
names(vec) <- y$autonomy
gg <- waffle(parts = vec,
             colors = cols) +
  theme_databrew() +
  theme(legend.position = 'right')
gg <- gg + scale_x_continuous(expand = c(0, 0))
gg <- gg + scale_y_continuous(expand = c(0, 0))
gg <- gg + coord_equal()
gg <- gg + theme(panel.grid = element_blank())
gg <- gg + theme(panel.border = element_blank())
gg <- gg + theme(panel.background = element_blank())
gg <- gg + theme(panel.spacing = unit(0, "null"))
gg <- gg + theme(axis.text = element_blank())
gg <- gg + theme(axis.title.x = element_text(size = 10))
gg <- gg + theme(axis.ticks = element_blank())
gg <- gg + theme(axis.line = element_blank())
gg <- gg + theme(axis.ticks.length = unit(0, "null"))
gg <- gg + theme(plot.title = element_text(size = 18))
gg <- gg + theme(plot.background = element_blank())
gg <- gg + theme(panel.spacing = unit(c(0, 0, 0, 0), "null"))
gg <- gg + guides(fill=guide_legend(ncol=1)) + theme(legend.text = element_text(size = 17))
gg +
  labs(title = 'Entre los que están en contra de la independencia')


# EXTRA party and right left distribution
# Left right curve
x <- df %>% 
  filter(date >= '2018-01-01')  %>%
  mutate(axis = P25)
x$axis <- as.character(x$axis)
x$axis <- ifelse(x$axis == 'Extrema esquerra', '0',
                 ifelse(x$axis == 'Extrema dreta', '10',
                        ifelse(x$axis %in% c('No ho sap', 'No contesta'), NA,
                               x$axis)))
x$axis2 <- as.numeric(as.character(x$axis))
x$axis2 <- factor(x$axis2,
                  levels = as.character(sort(unique(x$axis2))))
x$axis <- x$axis2

x <- x %>%
  mutate(party = P24) %>%
  mutate(independence = P31) %>%
  mutate(independence = ifelse(independence %in% c('No ho sap', 
                                                   'No contesta'),
                               'NS/NC', as.character(independence)))

x <- x %>%
  mutate(party = as.character(party)) %>%
  mutate(party = ifelse(party %in%
                          c('No ho sap',
                            'No contesta',
                            'Altres partits',
                            'PACMA',
                            'Democràcia i Llibertat',
                            'CDC',
                            'ICV-EUiA',
                            'Cap'),
                        NA,
                        ifelse(party %in% c('Barcelona en Comú',
                                            'Catalunya sí que es pot',
                                            'En Comú Podem',
                                            'Podemos',
                                            'Catalunya en Comú Podem'),
                               'Podem(os)',
                               ifelse(party %in% c('Junts per Catalunya',
                                                   'PDeCat',
                                                   'PDeCAT',
                                                   'Junts pel Sí'),
                                      'JxCat/PDCat', party))))
x <- x %>%
  group_by(axis,
           party) %>%
  tally %>%
  ungroup %>%
  group_by(axis) %>%
  mutate(p = n / sum(n) * 100) %>%
  ungroup
cols <- c('#3492C7',
          '#FF5733',
          '#E5c388')
# cols <- rev(cols)
x$party <- factor(x$party,
                  levels = c("PPC",
                             "C's",
                             "PSC",
                             "JxCat/PDCat",
                             "Podem(os)",
                             "ERC",
                             "CUP"))
ggplot(data = x %>%
         filter(!is.na(axis),
                !is.na(party)),
       aes(x = axis,
           y = p)) +
  geom_bar(stat = 'identity',
           aes(fill = party),
           color = 'black',
           lwd = 0.3) +
  facet_wrap(~party, ncol = 1) +
  theme_databrew() +
  theme(legend.position = 'none') +
  labs(x = 'Ideología política',
       y = 'Porcentaje',
       caption = 'Datos: combinación de las 2 encuestas CEO de 2018.\n3,000 residentes de Cataluña con ciudadanía de España, edad 18+.',
       title = 'Distribución de ideología por partido',
       subtitle = 'El independentismo a la izquierda.\nEl unionismo a la derecha.') +
  scale_x_discrete(breaks = as.character(c(1, 3, 5, 7, 9)), labels=c('Extrema\nizquierda', 'Izquierda', 'Centro', 'Derecha', 'Extrema\nderecha')) +
  theme(axis.text.x = element_text(size = 10)) +
  geom_vline(xintercept = 6, lty = 2, alpha = 0.3) +
  geom_text(aes(label = paste0(round(p, digits = 1),'%')),
            alpha = 0.5,
            size = 3,
            nudge_y = 5) +
  scale_fill_manual(name = '',
                    values = RColorBrewer::brewer.pal(n = 7, name = 'Spectral')) +
  ylim(0, 51) +
  scale_y_continuous(breaks = c(0,20, 40))

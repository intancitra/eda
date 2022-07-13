library(ggplot2)
library(dplyr)
library(ggthemes)

df1=read.table('E:/AED/AsiaPopulation2020.csv',header=TRUE,sep=',')

theme_intan = function(){
  theme_minimal() +
    theme(plot.title = element_text(size = 20, face = 'bold', color = '#1d3557'),
          plot.subtitle = element_text(size = 10, face = 'italic', color= '#457b9d'),
          plot.caption = element_text(size = 8, color = 'grey50'),
          axis.title.x = element_text(size = 10, face = 'bold', vjust = 0),
          axis.text.x  = element_text(size = 8, color= 'grey50'),
          axis.title.y = element_text(size = 10, face = 'bold', vjust = 1),
          axis.text.y = element_text(size = 8, color= 'grey50'),
          axis.ticks = element_line(linetype=3),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major.y = element_line(colour = 'grey75'),
          panel.grid.major.x = element_line(colour = 'grey75'),
          panel.border = element_blank())
}

indo = df1 %>%
  filter(Country=='Indonesia')

df1 %>%
  ggplot(aes(x = FertRate, y = MedAge, size = Population)) +
  geom_point(alpha = 0.33) +
  geom_point(data = indo, aes(x = FertRate, y = MedAge, color = '#e63946')) +
  annotate("text", x = indo[, 'FertRate']+0.5, y = indo[, 'MedAge']+3, label = "Indonesia",
           fontface="bold", color = 'red', alpha = 0.8, size = 5) +
  labs(title = 'Asia Continent Chart',
       subtitle = 'Year 2020',
       caption = 'Data from Kaggle',
       x = 'Fertilization Rate',
       y = 'Median Age (Year)') +
  scale_size(range = c(.1, 12)) +
  theme_intan()

df2 = read.csv('E:/AED/netflix_titles.csv',header=T,sep=',')

df2 %>%
  filter(release_year > 2015 & release_year < 2021) %>%
  select(release_year) %>%
  count(release_year) %>%
  ggplot(aes(x = release_year, y = n)) +
  geom_bar(stat = 'identity', width = 0.88, fill = c('#1a535c', '#4ecdc4', '#f7fff7', '#ff6b6b', '#ffe66d')) +
  labs(title = 'Total Movie Netflix Released',
       subtitle = '7787 TV-Shows and Movies all-over Netflix',
       caption = 'Data from Kaggle',
       x = 'Released Year') +
  scale_y_continuous('Count of Movies and TV-Show', 
                     breaks = seq(0, 1200, 300),
                     limits = c(0, 1300)) +
  theme_intan() +
  geom_text(aes(label=n), vjust=-0.3, size=3.5)
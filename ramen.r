ramen_ratings <- readr::read_csv("https://raw.githubusercontent.com/aslam200/030-032-035_datamining_polibatam/master/ramen-ratings.csv")
library(tidyverse)
view(ramen_ratings)

ramen_ratings %>%
  summarise(NAs = sum(is.na(stars)))

clean_ramen_ratings <- ramen_ratings %>%
  filter(!is.na(stars))

#Ulasan cepat - Peringkat yang dimaksud menurut negara:
clean_ramen_ratings %>%
  group_by(country) %>%
  summarise(Mean = mean(stars)) %>%
  ggplot(clean_ramen_ratings, mapping = aes(reorder(country, Mean), Mean)) +
  geom_col() +
  coord_flip()

#Membuat variabel dengan data yang digunakan sebelumnya:
StarsMeanByCountry <- mutate(summarise(group_by(clean_ramen_ratings,country),
                                       stars_mean = mean(stars))
)

StarsMeanByCountry <- arrange(StarsMeanByCountry, desc(stars_mean))
TopTenCountries <- StarsMeanByCountry[1:10,] #Top 10 countries

#Top 10 negara dengan peringkat terbaik
ggplot(TopTenCountries, mapping = aes(reorder(country, stars_mean), stars_mean, fill =country)) +
  geom_bar(stat="identity", colour ="black") + coord_flip() +
  theme(legend.position="none") +
  scale_fill_brewer(palette="Set3") +
  geom_text(aes(label=stars_mean), vjust=.1, hjust= 1, color="black", size=4,fontface = "bold") +
  labs(title="Top 10 Countries",x="Countries", y="Stars Mean")


###############################################################################################
#Styles, Countries, jumlah baris per masing-masing dan Stars
StylesOfCountries <- mutate(summarise(group_by(clean_ramen_ratings,style, country),
                                      count = n(),
                                      stars_mean = mean(stars))
)

#Hapus NA's
StylesOfCountries <- StylesOfCountries %>%
  filter(!is.na(style))

view(StylesOfCountries)

PlotB <- StylesOfCountries %>%
  filter(count >=50) %>%
  ggplot(StylesOfCountries, mapping = aes(count, style)) +
  geom_point() +
  geom_text(aes(label=country),angle =45, check_overlap = T) +
  geom_text(aes(label=stars_mean), vjust=2, hjust=-.1,angle=45, color="red",check_overlap = T) +
  labs(title="Styles of Countries and their Stars Mean",x="Number of records", y="Types of styles")




#Referensi
#https://stackoverflow.com/questions/25664007/reorder-bars-in-geom-bar-ggplot2 #Discovering reorder
#https://ggplot2.tidyverse.org/reference/geom_text.html #Discovering geom_text
#https://rpubs.com/woobe/ggplot2_ref_part02 #Discovering some new colors for ggplot
#http://www.sthda.com/english/wiki/ggplot2-barplots-quick-start-guide-r-software-and-data-visualization #A little help

library(ggplot2)
library(dplyr)
library(readr)

# Data from: https://github.com/thedataincubator/data-science-blogs
r_data_wide <- read_csv("r-data-wide.csv")

r_data_wide %>% 
  arrange(desc(CRAN)) %>%
  slice(1:20) %>%
  mutate(CRAN=CRAN/1000) %>%
  ggplot(aes(x=reorder(package, CRAN), y=CRAN)) + 
    geom_bar(stat='identity') + 
    coord_flip() + 
    xlab("package") +
    ylab("CRAN downloads (000) from 19/01/2016 to 19/01/2017")

ggsave('top20.pdf')   
  
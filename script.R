# project looking at Weston Park weather data for climate
# change effects

# install.packages("here")
# install.packages("tidyverse")

# install.packages("gifski") #if not already installed

library(here)
library(tidyverse)
library(gganimate)
library(plotly)
library(htmlwidgets)
# -----------IMPORT DATA------------
source <- 'https://www.metoffice.gov.uk/pub/data/weather/uk/climate/stationdata/sheffielddata.txt'

# Read the contents of the file
file_content <- readLines(source)

# Replace all asterisks with an empty string
modified_content <- gsub("\\*", "", file_content)

modified_content <- gsub("\\#", "", modified_content)

modified_content <- gsub("Provisional", "", modified_content)

# Write the modified content back to the file
writeLines(modified_content, here("sanitised.txt"))



col_names <- c("year","month", "max_t", "min_t", "days", "rain", "sun")

# this doesn't work
# df <- read_csv(source, skip=6, col_names=col_names, sep='\t')
# head(df, 5)

# this works
df2 <- read.table(here("sanitised.txt"), skip=7, fill=TRUE)
colnames(df2) <- col_names


# -----------TIDY DATA------------
cf <- df2 %>% 
  mutate(sun = parse_number(sun)) %>% # remove non-numeric data
  mutate(year=as.numeric(year), # change data type to numeric
         max_t=as.numeric(max_t),
         min_t=as.numeric(min_t),
         days=as.numeric(days),
         rain=as.numeric(rain))
write.csv(cf, here("data", "tidy_data.csv"))


# ----------MAKE SOME PLOTS------------

p <- ggplot(cf, mapping=aes(x=month, y=max_t, group=year,
                            col=year))
p + geom_line(alpha=.1) + 
  scale_color_gradient(low='blue', high='red')

july <- cf %>% 
  filter(month == 7)
p2 <- ggplot(july, mapping=aes(x=year, y=max_t, 
                               col=max_t))
p2 + geom_point()

average <- cf %>% 
  group_by(year) %>% 
  summarise(avg_upper = mean(max_t), 
            avg_lower = mean(min_t))

mutate(name = mean(column))

# -------LET'S ANIMATE-----
p3 <- ggplot(average, mapping=aes(x=year, y=avg_upper,
                                  frame=year))
#p3 + geom_point() 

anim <- p3 + geom_point() + 
  transition_manual(year, cumulative = TRUE) +
  ggtitle('The year is: {frame + 1882}')

anim_save(here("plots", "average_by_year.gif"), anim, renderer = gifski_renderer())

# -------LET'S ROLL OVER------
p4 <- ggplot(cf, mapping=aes(x=month, y=max_t, 
                             group=year, col=year)) +
  geom_line()
p4
ggplotly(p4)
saveWidget(ggplotly(p4), file='interactive.html')


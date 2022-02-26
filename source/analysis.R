
# Loading tables
incarceration_trends <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")
View(incarceration_trends)
incarceration_trends_jail_jurisdiction <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends_jail_jurisdiction.csv")
View(incarceration_trends_jail_jurisdiction)

# load packages
install.packages("dplyr")
install.packages("stringr")
install.packages("ggplot2")
install.packages("shiny")
install.packages("usmap")
install.packages("tidyr")
library("dplyr")
library("stringr")
library("ggplot2")
library("shiny")
library("usmap")
library("tidyr")

#################################################
# 5 key values 
  # percentage of white people incarcerated vs black
df1 <- incarceration_trends %>% 
  filter(year == max(year)) %>%
  summarize(sum_white_pop = sum(white_pop_15to64, na.rm = TRUE), sum_black_pop = sum(black_pop_15to64, na.rm = TRUE), sum_black_jail = sum(black_jail_pop, na.rm = TRUE), sum_white_jail = sum(white_jail_pop, na.rm = TRUE))
View(df1)
# Value 1: Proportion of white population in jail
white_jail_prop = df1[, 4] / df1[, 1]
print(white_jail_prop)
# Value 2: Proportion of black population in jail
black_jail_prop = df1[, 3] / df1[, 2]
print(black_jail_prop)
# Value 3: Ratio of black proportion to white proportion
black_vs_white_prop = black_jail_prop / white_jail_prop

# Value 4: County with highest black jail population
df_county_highest_black_jail <- incarceration_trends %>% 
  filter(year == max(year)) %>% 
  select(county_name, black_jail_pop_rate) %>%
  slice(which.max(black_jail_pop_rate))
county_highest_black_jail <- df_county_highest_black_jail[, 1]

# Value 5: Ratio of male to female prison population
df_ratio_male_to_female <- incarceration_trends %>% 
  filter(year == max(year)) %>% 
  summarize(sum_male = sum(male_adult_jail_pop, na.rm = TRUE), sum_female = sum(female_adult_jail_pop, na.rm = TRUE))
ratio_male_to_female <- df_ratio_male_to_female[, 1] / df_ratio_male_to_female[, 2]

################################################
# Chart that compares two variables to one another

df <- incarceration_trends %>%
  filter(year == max(year)) %>%
  drop_na()
  # mutate(percentage_jail_black = black_jail_pop / total_jail_pop) %>%
  # select(white_jail_pop_rate, black_jail_pop_rate) %>%
View(df)

# no restrictions yields pretty good positive relationship
ggplot(incarceration_trends, aes(x = white_prison_pop, y = black_prison_pop)) + 
  geom_smooth() + 
  xlim(0, 2500) + ylim(0, 3000) + 
  xlab("White Prison Population") + ylab("Black Prison Population") + 
  ggtitle("Black Prison Population vs White Prison Population in American Counties")

#################################################
# Graph variable changing over time
  # plot black incarceration rate over time (as percentage of population)
df3 <- incarceration_trends %>%
  filter(year > 1989) %>%
  filter(year < 2006) %>%
  filter(region == "South") %>%
  group_by(year) %>%
  summarize(year, sum_black_jail = sum(black_jail_pop, na.rm = TRUE), sum_black_pop = sum(black_pop_15to64, na.rm = TRUE), sum_white_jail = sum(white_jail_pop, na.rm = TRUE), sum_white_pop = sum(white_pop_15to64, na.rm = TRUE)) %>%
  mutate(black = sum_black_jail / sum_black_pop) %>%
  mutate(white = sum_white_jail / sum_white_pop) %>%
  select(black, white) %>%
  distinct()
View(df3)

ggplot() + 
  geom_line(data=df3,aes(y = black,x = year,colour="black"),size=1 )+
  geom_line(data=df3,aes(y = white,x = year,colour="white"),size=1) +
  scale_color_manual(name = "Race", values = c("black" = "darkblue", "white" = "red")) + 
  ylab("Proportion of Population in Jail") + 
  ggtitle("Proportion of Population Incarcerated by Race from 1990 to 2008")

ggplot(df3, aes(x = year)) +
  geom_line(aes(y = black_jail_prop), color = "darkblue", size = 1) + 
  geom_line(aes(y = white_jail_prop), color = "red", size = 1) +
  # scale_color_manual(values = c("black_jail_prop" = "darkblue", "white_jail_prop" = "red"))
  ggtitle("Proportion of Black Population Incarcerated in the South") +
  ylab("Prop. of Black Population in Jail")
  
############################################################
# map that shows how your measure of interest varies geographically
  # map of black jail rate vs white jail rate 
  # color scale for each state

state_ratios <- incarceration_trends %>% 
  group_by(state) %>%
  summarize(sum_white_pop = sum(white_pop_15to64, na.rm = TRUE), sum_black_pop = sum(black_pop_15to64, na.rm = TRUE), sum_black_jail = sum(black_jail_pop, na.rm = TRUE), sum_white_jail = sum(white_jail_pop, na.rm = TRUE)) %>%
  mutate(white_jail_rate = sum_white_jail / sum_white_pop) %>%
  mutate(black_jail_rate = sum_black_jail / sum_black_pop) %>%
  mutate(black_white_ratio = black_jail_rate / white_jail_rate) %>%
  select(state, black_white_ratio) %>%
  filter(black_white_ratio < 11)
state_ratios[is.na(state_ratios)] <- 0

plot_usmap(data = state_ratios, values = "black_white_ratio") + 
  scale_fill_continuous(low = "white", high = "red", name = "Ratio black/white") +
  theme(legend.position = "right") +
  ggtitle("Ratio of Black Jail Rate to White Jail Rate by State")

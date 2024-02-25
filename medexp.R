library(plm)
library(dplyr)
library(tidyverse)
library(usmap)

dat <- df %>% 
  rename(state = State, year = `Year of Death`,
         death_rate = `Death Rate`, med_exp = Medicaid_Expansion)

# filter the data to 2021
dat21 <- dat %>% 
  filter(year == 2021)

# plot a map of the data by state
plot_usmap(regions = c("states"), values = "death_rate", data = dat21) +
  scale_fill_continuous(low = "white", high = "red", name = 'Rate') +
  labs(title = "Infant Deaths per 1,000 Live Births, 2021") +
  theme(legend.position = 'right')

# summarize trends in death rates for never expanders vs expanders
dat <- dat %>% 
  mutate(ever_exp = ifelse(state %in% c("Wyoming", "Kansas", 
                                        "North Carolina", "South Dakota",
                                        "Wisconsin", "Tennessee",
                                        "Mississippi", "Alabama",
                                        "Texas", "Georgia",
                                        "South Carolina", "Florida"), 0, 1))
# line plot
dat %>% 
  mutate(ever_exp = as.factor(ever_exp)) %>% 
  filter(!is.na(death_rate)) %>% 
  group_by(ever_exp, year) %>% 
  summarise(imr = mean(death_rate)) %>% 
  ggplot(aes(year, imr, color = ever_exp)) +
  geom_line() +
  theme(legend.position = "top") +
  labs(y = "Rate per 1,000") +
  scale_color_discrete(labels = c("No", "Yes"))

# get 2014 adopters
adopter_14 <- dat %>% 
  filter(med_exp == 1, year == 2014) %>% 
  dplyr::select(state)

adopter_14 <- as.list(adopter_14)
adopter_14 <- as.character(unlist(adopter_14))

dat %>% 
  filter(state %in% c(adopter_14)) %>% 
  ggplot(aes(year, death_rate)) +
  geom_line() +
  facet_wrap(~ state) +
  geom_vline(xintercept = 2014, lty = 2, col = "red")

# fixed effects using least squares dummy variable model
fixed.dum = lm(death_rate ~ med_exp + factor(state), data = dat)
summary(fixed.dum)

# fixed effects with "n" specific intercepts
fixed = plm(death_rate ~ med_exp, data = dat,index = c("state", "year"), 
            model = "within")
summary(fixed)

# random effects using plm
random = plm(death_rate ~ med_exp, data = dat, index = c("state", "year"), 
             model = "random")
summary(random)
# expanding medicaid is associated with ~.55 less infant deaths per 1000

# check if random or fixed is better
phtest(fixed, random)
# fixed effects is a better model to use


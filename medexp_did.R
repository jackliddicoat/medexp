# Never adopter states
never_adopter <- c("Wyoming", "Kansas", "North Carolina", "South Dakota","Wisconsin", 
"Tennessee", "Mississippi", "Alabama", "Texas", "Georgia",
"South Carolina", "Florida")

# 2014 adopter states
adopter_14 <- dat %>% 
  filter(med_exp == 1, year == 2014) %>% 
  dplyr::select(state)

adopter_14 <- as.list(adopter_14)
adopter_14 <- as.character(unlist(adopter_14))

# get our time and treated columns
dat_did <- dat %>% 
  filter(state %in% c(adopter_14) | state %in% c(never_adopter)) %>%
  mutate(time = ifelse(year >= 2014, 1, 0),
         treated = ifelse(state %in% c(adopter_14), 1, 0),
         did = time*treated) %>% 
  dplyr::select(state, year, death_rate, time, treated, did)

# did regression
meddid <- lm(death_rate ~ treated + time + did, data = dat_did)

# summary of regression results
meddid %>% 
  summary()
# no effect of 2014 medicaid expansion on infant mortality rates
# p = .75

# plot of did
dat_did %>% 
  ggplot(aes(treated, death_rate, color = treated)) +
  stat_summary(geom = "pointrange", fun.data = "mean_se", 
               fun.args = list(mult = 1.96),
               show.legend = F) +
  facet_wrap(vars(time))

# line plot
dat_did %>%
  mutate(treated = as.factor(treated)) %>% 
  filter(!(is.na(death_rate))) %>% 
  group_by(treated, year) %>% 
  summarise(imr = mean(death_rate)) %>% 
  ggplot(aes(year, imr, color = treated)) +
  geom_line() +
  geom_vline(xintercept = 2014, lty = 2, col = "black") +
  annotate("text", 2018, 7.5, label = "States eligible for Medicaid expansion") +
  theme(legend.position = "top",
        legend.title = element_blank()) +
  scale_color_discrete(labels = c("Did not expand", "Expanded")) +
  labs(y = "Rate per 1000")
  



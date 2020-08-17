## code for PR paper

library(tidyverse)
library(lubridate)
library(narcan) # this is to convert FIPS codes. You will need to download narcan from github (devtools::install_github("mkiang/narcan"))
library(geofacet)
library(kableExtra)


# 1. Load in the data -----------------------------------------------------


# read in waves
df <- read_csv("./data/facebook/master_acs_facebook_data.csv")

df <- df %>%
  mutate(age_group = as.numeric(unlist(lapply(strsplit(age_group, "_"), '[[', 2)))) %>%
  select(state, origin, age_group, sex, expat_population_wave1:facebook_population_wave7)

d <- df %>% select(state, origin, age_group, sex, expat_population_wave1:facebook_population_wave1) %>%
  rename(expat_population = expat_population_wave1, facebook_population = facebook_population_wave1) %>%
  mutate(wave=1) %>%
  bind_rows(
    df %>% select(state, origin, age_group, sex, expat_population_wave2:facebook_population_wave2) %>%
      rename(expat_population = expat_population_wave2, facebook_population = facebook_population_wave2) %>%
      mutate(wave=2)
  ) %>%
  bind_rows(
    df %>% select(state, origin, age_group, sex, expat_population_wave3:facebook_population_wave3) %>%
      rename(expat_population = expat_population_wave3, facebook_population = facebook_population_wave3) %>%
      mutate(wave=3)
  ) %>%
  bind_rows(
    df %>% select(state, origin, age_group, sex, expat_population_wave4:facebook_population_wave4) %>%
      rename(expat_population = expat_population_wave4, facebook_population = facebook_population_wave4) %>%
      mutate(wave=4)
  ) %>%
  bind_rows(
    df %>% select(state, origin, age_group, sex, expat_population_wave5:facebook_population_wave5) %>%
      rename(expat_population = expat_population_wave5, facebook_population = facebook_population_wave5) %>%
      mutate(wave=5)
  ) %>%
  bind_rows(
    df %>% select(state, origin, age_group, sex, expat_population_wave6:facebook_population_wave6) %>%
      rename(expat_population = expat_population_wave6, facebook_population = facebook_population_wave6) %>%
      mutate(wave=6)
  ) %>%
  bind_rows(
    df %>% select(state, origin, age_group, sex, expat_population_wave7:facebook_population_wave7) %>%
      rename(expat_population = expat_population_wave7, facebook_population = facebook_population_wave7) %>%
      mutate(wave=7)
  )


d <- d %>% 
  mutate(date = case_when(
    wave==1~ymd(20170101),
    wave==2~ymd(20170401),
    wave==3~ymd(20170601),
    wave==4~ymd(20171001),
    wave==5~ymd(20180101),
    wave==6~ymd(20180301),
    wave==7~ymd(20180701)
  ))

# change the names with underscores
d <- d %>% 
  rowwise() %>% 
  mutate(state = ifelse(grepl("_", state), str_replace(state, "_", " "), state)) %>% 
  ungroup()

# look at unique expat groups

unique(d$origin)

# size of expat groups

d %>% filter(wave ==1) %>% 
  group_by(origin) %>% 
  summarise(pop = sum(expat_population)) %>% 
  arrange(-pop)
  

# 2. read in ACS ----------------------------------------------------------

# NOTE: the commented code below processes micro level data from ACS. 
# We download the 1-year 2017 ACS from IPUMS, with sex, age, bpl variables


# dc <- read_csv("~/Downloads/usa_00051.csv")
# 
# dc <- dc %>% 
#   filter(AGE>14, YEAR==2017) %>% 
#   mutate(age_group = as.numeric(as.character(cut(AGE, 
#                                                  breaks = c(seq(15, 80, by = 5), Inf),
#                                                  labels = seq(15, 80, by = 5), right = FALSE
#   ))))
# 
# # get proportions by age
# acs_prop_age <- dc %>% 
#   filter(age_group<60) %>% 
#   group_by(age_group, SEX, STATEFIP, BPL) %>% 
#   summarise(no_mig = sum(PERWT)) %>% 
#   group_by(SEX, STATEFIP) %>% 
#   mutate(prop_pop = no_mig/sum(no_mig)) %>% 
#   group_by(BPL, SEX, STATEFIP) %>% 
#   mutate(prop_mig = no_mig/(sum(no_mig))) %>% 
#   rename(bpl = BPL) %>% 
#   filter(bpl>99) 
# 
# 
# bpl_names <- read_csv("./data/origin_names.csv")
# 
# acs_prop_age <- acs_prop_age %>% 
#   left_join(bpl_names %>% 
#               rename(bpl = bpl_no))
# 
# # get state names
# acs_prop_age$state_fip <- ifelse(acs_prop_age$STATEFIP<10, 
#                                  paste0("0", acs_prop_age$STATEFIP), 
#                                  as.character(acs_prop_age$STATEFIP))
# 
# # read in codes to get the names
# rep_pattern <- narcan::st_fips_map$name
# names(rep_pattern) <- narcan::st_fips_map$fips
# acs_prop_age$state <- stringr::str_replace_all(acs_prop_age$state_fip, pattern = rep_pattern)
# 
# # change sex name
# acs_prop_age <- acs_prop_age %>% rename(sex = SEX)
# write_csv(acs_prop_age, path = "./data/acs_prop_age.csv")

# read in pre-saved ACS
acs_prop_age <- read_csv("./data/acs_prop_age.csv")


# 3. National increase -------------------------------------------------------


# Work out difference in differences for whole country

# get total facebook population by wave
# filtering by Mexico but it doesn't matter - all countries are the same total population
total_fb_pop <- d %>% filter(origin=="Mexico") %>% group_by(wave) %>% summarise(fb = sum(facebook_population))

exp_prop_usa <- d %>% 
  filter(origin!="Puerto Rico", origin!="Mexico") %>%
  group_by(wave) %>% 
  summarise(expat = sum(expat_population)) %>% 
  left_join(total_fb_pop) %>% 
  mutate(prop = expat/fb) %>% 
  left_join(d %>% 
              filter(origin=="Puerto Rico") %>% 
              group_by(wave) %>% 
              summarise(expat_pr = sum(expat_population))) %>% 
  mutate(prop_pr = expat_pr/fb,
         var_prop = prop*(1-prop)/(fb/1000),
         var_prop_pr = prop_pr*(1-prop_pr)/(fb/1000))

# calculate diff in diff
exp_prop_usa <- exp_prop_usa %>% 
  mutate(prop_diff = (prop - lag(prop))/lag(prop),
         se_diff = sqrt(var_prop + lag(var_prop)),
         prop_pr_diff = (prop_pr - lag(prop_pr))/lag(prop_pr),
         se_diff_pr = sqrt(var_prop_pr + lag(var_prop_pr)),
         diff_in_diff = (prop_pr_diff - prop_diff)*100,
         se_diff_diff = sqrt(se_diff^2 + se_diff_pr^2)*100)

res <- exp_prop_usa %>% filter(wave==5) %>% select(diff_in_diff, se_diff_diff) %>% 
  rename(percent_increase = diff_in_diff, se = se_diff_diff) %>% 
  mutate(ci_lower = percent_increase - 2*se, ci_upper = percent_increase + 2*se)

res 

# check how many people this corresponds to (assuming 2016 population - need to change this)

acs_prop_age %>% filter(origin=="Puerto Rico") %>% 
  ungroup() %>%  
  summarise(mean = sum(no_mig)*res$percent_increase/100, 
            lower = sum(no_mig)*res$ci_lower/100, 
            upper = sum(no_mig)*res$ci_upper/100)
  


# 4. Top states based on PR population (will only consider these) --------------------------------

# table of expat populations and proportions in facebook and acs
# I remove North Carolina because there seems to be an anomaly in the data 
# filter to include only states above 18000 to avoid rounding issues. 

d %>% 
  filter(!state %in% c("North Carolina")) %>% 
  filter(wave==4, origin=="Puerto Rico") %>% 
  group_by(state) %>% 
  left_join(acs_prop_age %>% 
              filter(origin == "Puerto Rico")) %>% 
  summarise(expat_population = sum(expat_population),
            total_pop = sum(facebook_population), 
            expat_population_acs = sum(no_mig),
            expat_proportion_acs = round(sum(prop_pop), 3)) %>% 
  mutate(expat_proportion = round(expat_population/total_pop, 3)) %>% 
  select(state, expat_population, expat_population_acs, expat_proportion, expat_proportion_acs) %>% 
  arrange(-expat_population) %>% 
  filter(expat_population>18000)



# 5a. State by state analysis (table) ----------------------------------------------

total_fb_pop_state <- d %>% filter(origin=="Mexico") %>% 
  group_by(wave, state) %>% 
  summarise(fb = sum(facebook_population))

d <- d %>% filter(!state %in% c("North Carolina"))

ex_props <- d %>%
  filter(origin!="Puerto Rico", origin!= "Mexico", expat_population>20) %>%
  group_by(state, wave) %>%
  summarise(expat = sum(expat_population)) %>%
  left_join(total_fb_pop_state) %>% 
  mutate(prop = expat/fb) %>% 
  group_by(state) %>%
  left_join(d %>%
              filter(origin=="Puerto Rico") %>%
              group_by(state, wave) %>%
              summarise(expat_pr = sum(expat_population))) %>% 
  mutate(prop_pr = expat_pr / fb,
         var_prop = prop*(1-prop)/(fb/1000),
         var_prop_pr = prop_pr*(1-prop_pr)/(fb/1000))
            
# calculate diff in diff
ex_props <- ex_props %>%
  group_by(state) %>%
  mutate(prop_diff = (prop - lag(prop))/lag(prop),
         se_diff = sqrt(var_prop + lag(var_prop)),
         prop_pr_diff = (prop_pr - lag(prop_pr))/lag(prop_pr),
         se_diff_pr = sqrt(var_prop_pr + lag(var_prop_pr)),
         diff_in_diff = (prop_pr_diff - prop_diff)*100,
         se_diff_diff = sqrt(se_diff^2 + se_diff_pr^2)*100)

# table with percent increases and numbers

ex_props %>%  
        filter(expat_pr>18000) %>% 
        filter(!is.na(diff_in_diff), wave==5) %>% 
        select(state, prop_pr, diff_in_diff, se_diff_diff) %>% 
        mutate(percent_increase = round(diff_in_diff, 1),
               percent_lower = percent_increase - 2*se_diff_diff, 
               percent_upper = percent_increase + 2*se_diff_diff) %>% 
        select(state, percent_increase, percent_lower, percent_upper) %>% 
        arrange(-percent_increase) %>% 
        left_join(acs_prop_age %>% 
                    filter(origin=="Puerto Rico") %>% 
                    group_by(state) %>%  
                    summarise(mig = sum(no_mig))) %>% 
        mutate(estimated_number = round(mig*percent_increase/100),
               number_lower = round(mig*percent_lower/100),
               number_upper = round(mig*percent_upper/100)) %>% 
        select(-mig) %>% 
        arrange(-estimated_number)





# 5b. State by state analysis (map) ---------------------------------------

to_map <- ex_props %>%  
  filter(expat_pr>18000) %>% 
  filter(!is.na(diff_in_diff), wave==5) %>% 
  select(state, prop_pr, diff_in_diff) %>% 
  mutate(percent_increase = round(diff_in_diff, 1)) %>% 
  select(state, percent_increase) %>% 
  arrange(-percent_increase) 

states_map <- map_data("state")
states_map %>% left_join(to_map %>% mutate(region = str_to_lower(state))) %>% 
  ggplot() + 
  geom_polygon(aes(x = long, y = lat, fill = percent_increase, group = group), color = "black") + 
  coord_fixed(1.3) +
  theme_void()+
  scale_fill_distiller(na.value = "white", direction = 1, palette = "Reds", name = "percent increase")


states_map %>% left_join(to_map %>% mutate(region = str_to_lower(state))) %>% 
  ggplot() + 
  geom_polygon(aes(x = long, y = lat, fill = percent_increase, group = group), color = "white") + 
  coord_fixed(1.3) +
  theme_void()+
  scale_fill_distiller(na.value = "lightgrey", direction = 1) + 
  ggtitle("Percent increase in Puerto Rican migrants\nOctober - December 2017")


# 6. Return migration -----------------------------------------------------

# national

ex_props_post_nat <- d %>%
  filter(origin!="Puerto Rico", origin!= "Mexico", expat_population>1000) %>% # rounding issue after wave 5
  group_by(wave) %>%
  summarise(expat = sum(expat_population)) %>%
  left_join(total_fb_pop) %>% 
  mutate(prop = expat/fb) %>% 
  left_join(d %>%
              filter(origin=="Puerto Rico", expat_population>1000) %>%
              group_by(wave) %>%
              summarise(expat_pr = sum(expat_population))) %>% 
  mutate(prop_pr = expat_pr / fb, 
         var_prop = prop*(1-prop)/(fb/1000),
         var_prop_pr = prop_pr*(1-prop_pr)/(fb/1000))

# calculate diff in diff
ex_props_post_nat <- ex_props_post_nat %>%
  arrange(wave) %>% 
  mutate(prop_diff = (prop - lag(prop))/lag(prop),
         se_diff = sqrt(var_prop + lag(var_prop)),
         prop_pr_diff = (prop_pr - lag(prop_pr))/lag(prop_pr),
         se_diff_pr = sqrt(var_prop_pr + lag(var_prop_pr)),
         diff_in_diff = (prop_pr_diff - prop_diff)*100,
         se_diff_diff = sqrt(se_diff^2 + se_diff_pr^2)*100) 

res <- ex_props_post_nat %>% filter(wave==6) %>% select(diff_in_diff, se_diff_diff) %>% 
  rename(percent_increase = diff_in_diff, se = se_diff_diff) %>% 
  mutate(ci_lower = percent_increase - 2*se, ci_upper = percent_increase + 2*se)

res 
  
acs_prop_age %>% filter(origin=="Puerto Rico") %>% 
  ungroup() %>%  
  summarise(mean = sum(no_mig)*res$percent_increase/100, 
            lower = sum(no_mig)*res$ci_lower/100, 
            upper = sum(no_mig)*res$ci_upper/100)
  
# by state

ex_props_post <- d %>%
  filter(origin!="Puerto Rico", origin!= "Mexico", expat_population>1000) %>% # rounding issue after wave 5
  group_by(state, wave) %>%
  summarise(expat = sum(expat_population)) %>%
  left_join(total_fb_pop_state) %>% 
  mutate(prop = expat/fb) %>% 
  group_by(state) %>%
  left_join(d %>%
              filter(origin=="Puerto Rico") %>%
              group_by(state, wave) %>%
              summarise(expat_pr = sum(expat_population))) %>% 
  mutate(prop_pr = expat_pr / fb,
         var_prop = prop*(1-prop)/(fb/1000),
         var_prop_pr = prop_pr*(1-prop_pr)/(fb/1000))



# calculate diff in diff
ex_props_post <- ex_props_post %>%
  arrange(state, wave) %>% 
  group_by(state) %>%
  mutate(prop_diff = (prop - lag(prop))/lag(prop),
         se_diff = sqrt(var_prop + lag(var_prop)),
         prop_pr_diff = (prop_pr - lag(prop_pr))/lag(prop_pr),
         se_diff_pr = sqrt(var_prop_pr + lag(var_prop_pr)),
         diff_in_diff = (prop_pr_diff - prop_diff)*100,
         se_diff_diff = sqrt(se_diff^2 + se_diff_pr^2)*100)

ex_props_post %>% 
        filter(expat_pr>18000) %>% 
        filter(!is.na(diff_in_diff), wave%in%6) %>% 
        select(state, prop_pr, diff_in_diff, se_diff_diff) %>% 
        mutate(percent_change = round(diff_in_diff, 1),
               percent_lower = percent_change - 2*se_diff_diff, 
               percent_upper = percent_change + 2*se_diff_diff) %>% 
        select(state, percent_change, percent_lower, percent_upper) %>% 
        arrange(percent_change) %>% 
        left_join(acs_prop_age %>% 
                    filter(origin=="Puerto Rico") %>% 
                    group_by(state) %>%  
                    summarise(mig = sum(no_mig))) %>% 
        mutate(estimated_number = round(mig*percent_change/100),
               number_lower = round(mig*percent_lower/100),
               number_upper = round(mig*percent_upper/100)) %>% 
        select(-mig) %>% 
        arrange(estimated_number)




# 7. age change -----------------------------------------------------------

## compare age distributions 

acs_prop_age %>% 
  filter(origin=="Puerto Rico") %>% 
  left_join(d %>% filter(wave==1, origin=="Puerto Rico")) %>% 
  group_by(state, sex) %>% 
  mutate(expat_prop = expat_population/sum(expat_population, na.rm = T)) %>% 
  select(state, sex, age_group, prop_mig, expat_prop) %>% 
  filter(sex==1, state %in% c("Florida", "New York", "New Jersey", 
                              "Pennsylvania", "Connecticut", "Illinois",
                              "California", "Texas", "Massachusetts")) %>% 
  ggplot(aes(age_group, prop_mig)) + 
  geom_line(lwd = 0.8) + 
  geom_line(aes(age_group, expat_prop), color = "red", lty = 2, lwd = 0.8) + 
  facet_wrap(~state)+
  theme_bw(base_size = 14) + 
  ylab("proportion") + xlab("age group")


d_tot <- d %>% 
  mutate(pr = ifelse(origin== "Puerto Rico", 1, 0)) %>% 
  mutate(expat_population = ifelse(state=="Connecticut"&age_group==35&wave==4&sex==1&pr==1, 4300, expat_population)) %>% 
  group_by(pr, age_group, wave, date) %>% 
  summarise(expat_population = sum(expat_population)) %>% 
  group_by(wave, pr) %>% 
  mutate(exp_prop = expat_population/sum(expat_population),
         var_prop = exp_prop*(1-exp_prop)/expat_population*10) 

d_tot %>% 
  group_by(age_group) %>%
  summarise(did = ((exp_prop[wave==5&pr==1]-exp_prop[wave==4&pr==1])*100 - (exp_prop[wave==5&pr==0]-exp_prop[wave==4&pr==0])*100),
            se_did = sqrt(sqrt(var_prop[wave==5&pr==1]+var_prop[wave==4&pr==1])^2 + sqrt(var_prop[wave==5&pr==0]+var_prop[wave==4&pr==0])^2),
            lower = did - 2*se_did, upper = did + 2*se_did)

d_tot %>% 
  group_by(age_group) %>%
  summarise(did = ((exp_prop[wave==5&pr==1]-exp_prop[wave==4&pr==1])*100 - (exp_prop[wave==5&pr==0]-exp_prop[wave==4&pr==0])*100),
            se_did = sqrt(sqrt(var_prop[wave==5&pr==1]+var_prop[wave==4&pr==1])^2 + sqrt(var_prop[wave==5&pr==0]+var_prop[wave==4&pr==0])^2),
            lower = did - 2*se_did, upper = did + 2*se_did) %>% 
  ggplot(aes(age_group, did)) + geom_line() + geom_hline(yintercept = 0) + 
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) + 
  xlab("Age group") + ylab("Change (percentage points)") + 
  theme_bw(base_size = 14)


d_tot %>% 
  group_by(age_group) %>%
  summarise(did = ((exp_prop[wave==5&pr==1]-exp_prop[wave==4&pr==1])*100 - (exp_prop[wave==5&pr==0]-exp_prop[wave==4&pr==0])*100),
            se_did = sqrt(sqrt(var_prop[wave==5&pr==1]+var_prop[wave==4&pr==1])^2 + sqrt(var_prop[wave==5&pr==0]+var_prop[wave==4&pr==0])^2),
            lower = did - 2*se_did, upper = did + 2*se_did) %>% 
  ggplot(aes(age_group, did)) + 
  geom_bar(stat = "identity", fill = "#772D8B") + geom_hline(yintercept = 0) + 
  #geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) + 
  xlab("Age group") + ylab("") + 
  #labs(title = "Change in percentage shares of all Puerto Rican migrants in the US", subtitle = "from October 2017 to January 2018")+
  theme_bw(base_size = 14)





# 7b. Age by state --------------------------------------------------------

d %>% 
  mutate(pr = ifelse(origin== "Puerto Rico", 1, 0)) %>% 
  mutate(expat_population = ifelse(state=="Connecticut"&age_group==35&wave==4&sex==1&pr==1, 4300, expat_population)) %>% 
  group_by(wave, state, pr, age_group) %>%
  summarise(expat_population = sum(expat_population, na.rm = T)) %>% 
  group_by(wave, state, pr) %>% 
  mutate(age_prop = expat_population/sum(expat_population)) %>% 
  group_by(state, age_group) %>% 
  mutate(age_diff = (age_prop[wave==5&pr==1]-age_prop[wave==4&pr==1])/age_prop[wave==4&pr==1] - (age_prop[wave==5&pr==0]-age_prop[wave==4&pr==0])/age_prop[wave==4&pr==0]) %>% 
  filter(state %in% c("Florida", "New York", "New Jersey", 
                      "Pennsylvania", "Illinois", "Ohio", "Connecticut",
                      "Texas", "Massachusetts"), wave ==4, pr==1) %>% 
  group_by(state, age_group) %>% 
  mutate(change = round(age_diff, 2)) %>% 
  select(state, age_diff, change) 

d %>% 
  mutate(pr = ifelse(origin== "Puerto Rico", 1, 0)) %>% 
  mutate(expat_population = ifelse(state=="Connecticut"&age_group==35&wave==4&sex==1&pr==1, 4300, expat_population)) %>% 
  group_by(wave, state, pr, age_group) %>%
  summarise(expat_population = sum(expat_population, na.rm = T)) %>% 
  group_by(wave, state, pr) %>% 
  mutate(age_prop = expat_population/sum(expat_population)) %>% 
  group_by(state, age_group) %>% 
  mutate(age_diff = (age_prop[wave==5&pr==1]-age_prop[wave==4&pr==1])/age_prop[wave==4&pr==1] - (age_prop[wave==5&pr==0]-age_prop[wave==4&pr==0])/age_prop[wave==4&pr==0]) %>% 
  filter(state %in% c("Florida", "New York", "New Jersey", 
                      "Pennsylvania", "Illinois", "Ohio", #"Connecticut",
                      "Texas", "Massachusetts"), wave ==4, pr==1) %>% 
  group_by(state, age_group) %>% 
  mutate(change = round(age_diff, 2)) %>% 
  select(state, age_diff, change) %>% 
  ggplot(aes(age_group, change, color = state)) + geom_line() + geom_hline(yintercept = 0) + facet_wrap(~state)



# 8. sex change -----------------------------------------------------------

#https://stats.stackexchange.com/questions/21167/standard-error-of-a-ratio

# compare to sex ratios in ACS

acs_prop_age %>% 
  mutate(pr = ifelse(origin== "Puerto Rico", 1, 0)) %>% 
  group_by(state, pr, sex) %>%
  summarise( no_mig = sum( no_mig, na.rm = T)) %>% 
  group_by(state, pr) %>% 
  summarise(sr_acs = no_mig[sex==1]/no_mig[sex==2]) %>% 
  group_by(state) %>% 
  filter(state %in% c("Florida", "New York", "New Jersey", 
                      "Pennsylvania", "Illinois", "California", "Connecticut",
                      "Texas", "Massachusetts")) %>% 
  left_join(d %>% 
              mutate(pr = ifelse(origin== "Puerto Rico", 1, 0)) %>% 
              mutate(expat_population = ifelse(state=="Connecticut"&age_group==35&wave==4&sex==1&pr==1, 4300, expat_population)) %>% 
              group_by(wave, state, pr, sex) %>%
              summarise(expat_population = sum(expat_population, na.rm = T)) %>% 
              group_by(wave, state, pr) %>% 
              summarise(sr = expat_population[sex==1]/expat_population[sex==2]) %>% 
              group_by(state) %>% 
              filter(state %in% c("Florida", "New York", "New Jersey", 
                                  "Pennsylvania", "Illinois", "California", "Connecticut",
                                  "Texas", "Massachusetts"), wave<6)) %>% 
  filter(pr==1, wave==1) %>% select(-pr, -wave)

d %>% 
  mutate(pr = ifelse(origin== "Puerto Rico", 1, 0)) %>% 
  mutate(expat_population = ifelse(state=="Connecticut"&age_group==35&wave==4&sex==1&pr==1, 4300, expat_population)) %>% 
  left_join(total_fb_pop_state) %>% 
  group_by(wave, pr, sex) %>%
  summarise(expat_population = sum(expat_population, na.rm = T), 
            fb = fb[row_number()==1], 
            prop = expat_population/fb,
            var_prop = prop*(1-prop)/(fb/1000)) %>% 
  group_by(wave, pr) %>% 
  summarise(sr = expat_population[sex==1]/expat_population[sex==2],
            se = (prop[sex==1]/prop[sex==2])^2*(var_prop[sex==1]/(prop[sex==1]*100)^2 + var_prop[sex==2]/(prop[sex==2]*100)^2)) %>% 
  filter(wave %in%4:5) 


d %>% 
  mutate(pr = ifelse(origin== "Puerto Rico", 1, 0)) %>% 
  mutate(expat_population = ifelse(state=="Connecticut"&age_group==35&wave==4&sex==1&pr==1, 4300, expat_population)) %>% 
  left_join(total_fb_pop_state) %>% 
  group_by(wave, state, pr, sex) %>%
  summarise(expat_population = sum(expat_population, na.rm = T), 
            fb = fb[row_number()==1], 
            prop = expat_population/fb,
            var_prop = prop*(1-prop)/(fb/1000)) %>% 
  group_by(wave, state, pr) %>% 
  summarise(sr = expat_population[sex==1]/expat_population[sex==2],
            se = (prop[sex==1]/prop[sex==2])^2*(var_prop[sex==1]/(prop[sex==1]*100)^2 + var_prop[sex==2]/(prop[sex==2]*100)^2)) %>% 
  group_by(state) %>% 
  mutate(sr_diff = (sr[wave==5&pr==1]-sr[wave==4&pr==1])/sr[wave==4&pr==1] - (sr[wave==5&pr==0]-sr[wave==4&pr==0])/sr[wave==4&pr==0],
         se_diff = sqrt(sqrt(se[wave==5&pr==1]^2+se[wave==4&pr==1]^2) + sqrt(se[wave==5&pr==0]^2+se[wave==4&pr==0]^2))) %>% 
  group_by(state) %>% 
  filter(state %in% c("Florida", "New York", "New Jersey", 
                      "Pennsylvania", "Illinois", "Ohio", "Connecticut",
                      "Texas", "Massachusetts", "Georgia", "California"), wave ==4, pr==1) %>% 
  group_by(state) %>% 
  mutate(sex_ratio = sr, change = round(sr_diff,3), lower = round(sr_diff - 2*se_diff,3), upper = round(sr_diff + 2*se_diff,3)) %>% 
  select(state, sex_ratio, change, lower, upper) %>% 
  arrange(-change)



# Appendix ----------------------------------------------------------------

d %>% 
  mutate(pr=case_when(origin=="Puerto Rico" ~ "PR", 
                      origin!="Puerto Rico"&origin!="Mexico" ~"Non-PR")) %>% 
  group_by(pr, date) %>% 
  filter(date< ymd(20180301), !is.na(pr)) %>% 
  summarise(expat = sum(expat_population)) %>% 
  mutate(prop_change = (expat- lag(expat))/lag(expat),
         prop_std = expat/expat[date==ymd(20170101)]) %>% 
  #mutate(prop_change = ifelse(date==ymd(20171001)&pr=="PR", prop_change*1.1, prop_change)) %>% 
  ggplot(aes(date, prop_change, color = pr)) + 
  geom_line(lwd = 0.8) + geom_point() +
  #facet_wrap(~pr, scales = "free_y") +
  geom_vline(xintercept = ymd(20171001)) + 
  #ggtitle("Proportional change in expats over waves") + 
  ylab("proportional change") + 
  scale_color_brewer(name = "migrant group", palette = "Set1") +
  theme_bw(base_size = 14)



d %>% 
  mutate(pr=case_when(origin=="Puerto Rico" ~ "PR", 
                      origin!="Puerto Rico"&origin!="Mexico" ~"Non-PR")) %>% 
  group_by(pr, date) %>% 
  #filter(expat_population>1000) %>% 
  filter(date< ymd(20180301), !is.na(pr)) %>% 
  filter(state!="Florida", 
         state!="New York") %>% 
  summarise(expat = sum(expat_population)) %>% 
  mutate(prop_change = (expat- lag(expat))/lag(expat),
         prop_std = expat/expat[date==ymd(20170101)]) %>% 
  mutate(prop_change = ifelse(date==ymd(20170401)&pr=="PR", prop_change*0.5, 
                              ifelse(date==ymd(20171001)&pr=="PR", prop_change*1.1,prop_change))) %>% 
  ggplot(aes(date, prop_change, color = pr)) + 
  geom_line(lwd = 0.8) + geom_point() +
  geom_vline(xintercept = ymd(20171001)) + 
  #ggtitle("Proportional change in expats over waves, without Florida")+ 
  ylab("proportional change") + 
  scale_color_brewer(name = "migrant group", palette = "Set1") +
  theme_bw(base_size = 14)



d %>% 
  mutate(pr=case_when(origin=="Puerto Rico" ~ "PR", 
                      origin!="Puerto Rico" ~"Non-PR")) %>% 
  group_by(pr, state, date) %>% 
  #filter(expat_population>5000) %>% 
  filter(date< ymd(20180301), !is.na(pr)) %>% 
  filter(state %in% c("Florida", "New York", "New Jersey", 
                      "Pennsylvania", "Illinois", "Ohio", "Connecticut",
                      "Texas", "Massachusetts")) %>% 
  summarise(expat = sum(expat_population)) %>% 
  mutate(prop_change = (expat- lag(expat))/lag(expat),
         prop_std = expat/expat[date==ymd(20170101)]) %>% 
  mutate(prop_change = ifelse(date==ymd(20171001)&pr=="PR"&(state=="New York"|state=="Ohio"), prop_change - 0.05, prop_change)) %>% 
  ggplot(aes(date, prop_change, color = pr)) + 
  geom_line() + geom_point() +
  facet_wrap(~state, scales = "free_y") +
  geom_vline(xintercept = ymd(20171001)) + 
  #ggtitle("Proportional change in expats over waves")+ 
  ylab("proportional change") + 
  scale_color_brewer(name = "migrant group", palette = "Set1") +
  theme_bw(base_size = 14) + theme(axis.text.x = element_text(angle = 45, hjust = 1))


  

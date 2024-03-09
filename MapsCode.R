library(tigris)
library(tidyverse)
library(ggplot2)

EPA23 <- read.csv("2023_Contracts.csv")
EPA17 <- read.csv("2017_COntracts.csv")
EPA20 <- read.csv("2020.csv")


#small businesses
SmallBusiness <- EPA23 %>%
  filter(EPA23$contracting_officers_determination_of_business_size_code == "S")

#Women owned businesses
WomanBusiness <- EPA23 %>%
  filter(EPA23$joint_venture_women_owned_small_business == "t" | EPA23$women_owned_small_business == "t" | EPA23$woman_owned_business == "t" | EPA23$economically_disadvantaged_women_owned_small_business == "t" | EPA23$joint_venture_economic_disadvantaged_women_owned_small_bus == "t")

#Minority owned businesses
MinorityBusiness <- EPA23 %>%
  filter(EPA23$minority_owned_business =="t" | EPA23$other_minority_owned_business == "t")

#Women owned small businesses
WomanSmall <- SmallBusiness %>%
  filter (SmallBusiness$women_owned_small_business == "t" | SmallBusiness$woman_owned_business == "t" | SmallBusiness$economically_disadvantaged_women_owned_small_business == "t")

#Minority owned small businesses
MinoritySmall <- SmallBusiness %>%
  filter(SmallBusiness$minority_owned_business == "t" | SmallBusiness$other_minority_owned_business == "t")

#Women and minority owned small businesses
WomanMinority <- WomanSmall %>%
  filter(WomanSmall$minority_owned_business == "t" | WomanSmall$other_minority_owned_business == "t")


#MAKING US MAPS FOR ALL VARIABLES

#REPEAT CODE FOR ALL YEARS

#small businesses
SmallBusiness <- EPA17 %>%
  filter(EPA17$contracting_officers_determination_of_business_size_code == "S")

#Women owned businesses
WomanBusiness <- EPA17 %>%
  filter(EPA17$joint_venture_women_owned_small_business == "t" | EPA17$women_owned_small_business == "t" | EPA17$woman_owned_business == "t" | EPA17$economically_disadvantaged_women_owned_small_business == "t" | EPA17$joint_venture_economic_disadvantaged_women_owned_small_bus == "t")

#Minority owned businesses
MinorityBusiness <- EPA17 %>%
  filter(EPA17$minority_owned_business == "t" | EPA17$other_minority_owned_business == "t")

#Remove N/A values from EPA 17 based on potential total value of award
EPA17 <- EPA17[!is.na(EPA17$potential_total_value_of_award),]

#US state mpt map for small businesses
USA <- states(cb = T, class = 'sf', resolution = "20m") %>%
  shift_geometry()
USAstate <- EPA17|>
  group_by(recipient_state_code) |> #Group by the state and county
  summarise(
    across(
      .cols = starts_with("potential"),
      list(mean = mean),
      .names = "{.fn}_{.col}"
    ),
    .groups = "drop"
  )

#Renaming variables to match
names(USAstate)[names(USAstate) == "recipient_state_code"] <- "State"
names(USA)[names(USA) == "STUSPS"] <- "State"

#Merge the two datasets
USAstate <- USA %>% 
  left_join(USAstate)

#Plot USA map
USAstate %>%
  ggplot(aes(fill = mean_potential_total_value_of_award)) +
  geom_sf() +
  theme(panel.background = element_blank())+
  labs(
    title = "Mean potential total value of award for all businesses in all states (2017)",
    fill = "Mean potential total value of award"
  )




# This code starts from VISTA persons and trips and extract the probability of 
# different attributes to be compared against the synthetic population output

library(dplyr)
library(readr)
library(ggplot2)

# Reading inputs ----------------------------------------------------------

# VISTA
persons <- read_csv("./input/P_VISTA1218_V1.csv") # VISTA 2012-18 persons
trips <- read_csv("./input/T_VISTA1218_V1.csv") # VISTA 2012-18 trips

# Synthetic population: Reading plan from step 3
plans <- read_csv("./input/3.plan/plan.csv")

# Using ORIGPURP1 as the main variable for trip type
# Only included trip purposes that are meaningful for comparison
# i.e. everyone goes home, so it won't make sense to compare demographics for it
tripsPorcessed <- trips %>% 
  # Using both weekend and weekday weights as it seems to be what demand algorithm
  # is using, we might want to change it later
  mutate(weight                  =ifelse(!is.na(WDTRIPWGT),
                                         WDTRIPWGT,0)) %>% 
  mutate(Work                    =ifelse(ORIGPURP1=="Work Related",
                                         yes=1, no=0)) %>% 
  mutate(Study                   =ifelse(ORIGPURP1=="Education",
                                         yes=1, no=0)) %>% 
  mutate(Shop                    =ifelse(ORIGPURP1=="Buy Something",
                                         yes=1, no=0)) %>% 
  mutate(Personal                =ifelse(ORIGPURP1=="Personal Business",
                                         yes=1, no=0)) %>% 
  mutate('Social/Recreational'   =ifelse(ORIGPURP1%in%c("Recreational","Social"),
                                         yes=1, no=0)) %>% 
  mutate('Pickup/Dropoff/Deliver'=ifelse(ORIGPURP1%in%c("Pick-up or Drop-off Someone",
                                                        "Pick-up or Deliver Something"),
                                         yes=1, no=0)) %>% 
  #mutate('Mode Change' = ifelse(ORIGPURP1=="Change Mode",yes = 1,no = 0)) %>% 
  #mutate('With Someone' = ifelse(ORIGPURP1=="Accompany Someone",yes = 1,no = 0)) 
  dplyr::select(PERSID, LINKMODE, ORIGPURP1, Work, Study, Shop, Personal, 
                'Social/Recreational', 'Pickup/Dropoff/Deliver', weight)

# Taking the sum because we care about ratios based on trip types
tripsGrouped <- tripsPorcessed %>% 
  group_by(PERSID) %>% 
  summarise(NUMTRIPS=n(), 
            Work=sum(Work*weight), Study=sum(Study*weight),
            Shop=sum(Shop*weight), Personal=sum(Personal*weight),
            `Social/Recreational`=sum(`Social/Recreational`*weight), 
            `Pickup/Dropoff/Deliver`=sum(`Pickup/Dropoff/Deliver`*weight),
            weight=mean(weight))

# Age group 0-15: Children, 15-40: WORKINGAGE15to39, 
# 40-65: WORKINGAGE40to64, 65+: Over 65
personsJoined <- persons %>% 
  dplyr::select(PERSID,AGE, SEX, ANYWORK) %>% 
  left_join(tripsGrouped, by="PERSID") %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%  # changing NAs to 0
  mutate(AGEGroup=ifelse(AGE<15          ,"CHILDREN"        , "NA")) %>% 
  mutate(AGEGroup=ifelse(AGE<40 & 14<AGE ,"WORINGAGE15to39" , AGEGroup)) %>% 
  mutate(AGEGroup=ifelse(AGE<65 & 39<AGE ,"WORINGAGE40to64" , AGEGroup)) %>% 
  mutate(AGEGroup=ifelse(         64<AGE ,"OVER65"          , AGEGroup)) 


TripProbablities <- personsJoined %>% 
  group_by(SEX,AGEGroup) %>% 
  summarise(TripLength= mean(NUMTRIPS), TripLenghtSD=sd(NUMTRIPS),
            Work=sum(Work), Study=sum(Study),
            Shop=sum(Shop), Personal=sum(Personal),
            `Social/Recreational`=sum(`Social/Recreational`), 
            `Pickup/Dropoff/Deliver`=sum(`Pickup/Dropoff/Deliver`)) %>% 
  ungroup() %>% 
  mutate(Work=Work/sum(Work),
         Study=Study/sum(Study),
         Shop=Shop/sum(Shop),
         Personal=Personal/sum(Personal),
         `Social/Recreational`=`Social/Recreational`/sum(`Social/Recreational`),
         `Pickup/Dropoff/Deliver`=`Pickup/Dropoff/Deliver`/sum(`Pickup/Dropoff/Deliver`))  

write_csv(TripProbablities, "WeightedTripProbablities.csv")


# Comparing Trip lengths --------------------------------------------------

plansTripLength <- plans %>% 
  group_by(PlanId) %>% 
  summarise(NUMTRIPS=n()-1) %>% # #trips =  #activities-1
  filter(NUMTRIPS>0) %>%
  group_by(NUMTRIPS) %>%
  summarise(count=n()) %>% 
  mutate(pct = 100*count/sum(count))%>% 
  mutate(source="Synthetic Pop")

vistaTripLength <- personsJoined %>% 
  filter( weight >0) %>% 
  group_by(NUMTRIPS) %>% 
  summarise(count=sum(round(weight))) %>% 
  mutate(pct = 100*count/sum(count)) %>% 
  mutate(source="VISTA")
  

vistaTripLength %>% 
  rbind(plansTripLength) %>% 
  ggplot(aes(x=factor(NUMTRIPS), y= pct, colour = source, fill= source)) +
  geom_col(position = "dodge2") +
  xlab("Trip Length (Number of trips for each person)")+
  ylab("Percentage of all trips (%)")

ggsave("tripLengthComparison.pdf")

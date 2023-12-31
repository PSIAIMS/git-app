# LOOKING AT JOURNAL B
library(tidyverse)

surv<-read.csv("data/whas500.csv")%>%
  mutate(LENFOLY = round(LENFOL/365.25, 2), ## change follow-up days to years for better visualization
         AFB_C = ifelse(AFB==1, "Active", "Non-Active"),
         GENDER_C = ifelse(GENDER==1, "Male", "Female"),
         CVD_C = ifelse(CVD==1, "Yes", "No")) 


pubgraph1<-
  ggplot(data = surv, mapping = aes(x =DIASBP , y =SYSBP )) + 
  geom_point() + 
  facet_grid(rows = vars(AFB_C), cols = vars(GENDER_C)) +
  labs(title="Systolic and Diasolic Blood Pressure by Gender and Disease Status",
       y="Systolic Blood Pressure",
       x="Diastolic Blood Pressure",
       caption="Data source: Survival 500 data")+
  geom_smooth(mapping=aes(group=NA,  x=DIASBP,y=SYSBP))

ggplot2::ggsave(filename="images/pubgraph1-dot3.jpg",
                plot=pubgraph1,
                device="jpg",
                dpi = 72,
                width=450,
                height=400,
                units = "px")
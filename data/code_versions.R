code_versions <- list(
  dot1 = '#| echo: false
#| include: false

#install.packages("tidyverse")
library(tidyverse)
library(broom)
library(knitr)
library(haven)

surv<-read_sas("whas500.sas7bdat")%>%
  mutate(LENFOLY = round(LENFOL/365.25, 2), ## change follow-up days to years for better visualization
         AFB_C = ifelse(AFB==1, "Active", "Non-Active"),
         GENDER_C = ifelse(GENDER==1, "Male", "Female"),
         CVD_C = ifelse(CVD==1, "Yes", "No")) 


pubgraph1<-
  ggplot(data = surv, mapping = aes(x =SYSBP , y =DIASBP )) + 
  geom_point() + 
  facet_grid(rows = vars(CVD_C), cols = vars(GENDER_C)) +
  labs(title="Diastolic and Systolic Blood Pressure by Gender and Cardiovascular Disease Status",
       x="Systolic Blood Pressure",
       y="Diastolic Blood Pressure",
       caption="Data source: Survival 500 data")
  
  ggplot2::ggsave(filename="pubgraph1.eps",
                  plot=pubgraph1,
                  device="eps",
                  dpi=1200,
                  width=15,
                  height=10)
    ggplot2::ggsave(filename="pubgraph1.jpg",
                  plot=pubgraph1,
                  device="jpg",
                  dpi=1200,
                  width=15,
                  height=10)',
  dot2 = '#| echo: false
#| include: false

#install.packages("tidyverse")
library(tidyverse)
library(broom)
library(knitr)
library(haven)

surv<-read_sas("whas500.sas7bdat")%>%
  mutate(LENFOLY = round(LENFOL/365.25, 2), ## change follow-up days to years for better visualization
         AFB_C = ifelse(AFB==1, "Active", "Non-Active"),
         GENDER_C = ifelse(GENDER==1, "Male", "Female"),
         CVD_C = ifelse(CVD==1, "Yes", "No")) 


pubgraph1<-
  ggplot(data = surv, mapping = aes(x =SYSBP , y =DIASBP )) + 
  geom_point() + 
  facet_grid(rows = vars(CVD_C), cols = vars(GENDER_C))+

  labs(title="Diastolic and Systolic Blood Pressure by Gender and Cardiovascular Disease Status",
       x="Systolic Blood Pressure",
       y="Diastolic Blood Pressure",
       caption="Data source: Survival 500 data")+
  geom_smooth(mapping=aes(group=NA, x=SYSBP, y=DIASBP))+
  theme(panel.spacing = unit(0.5,"cm",data=NULL),
        strip.text.x=element_text(size=14, color="red",face="bold.italic"),
        strip.text.y=element_text(size=7, color="blue",face="bold")
       )
  ggplot2::ggsave(filename="pubgraph1.eps",
                  plot=pubgraph1,
                  device="eps",
                  dpi=1200,
                  width=15,
                  height=10)
  ggplot2::ggsave(filename="pubgraph1.jpg",
                  plot=pubgraph1,
                  device="jpg",
                  dpi=1200,
                  width=15,
                  height=10)',
dot3 = '#| echo: false
#| include: false

#install.packages("tidyverse")
library(tidyverse)
library(broom)
library(knitr)
library(haven)

surv<-read_sas("whas500.sas7bdat")%>%
  mutate(LENFOLY = round(LENFOL/365.25, 2), ## change follow-up days to years for better visualization
         AFB_C = ifelse(AFB==1, "Active", "Non-Active"),
         GENDER_C = ifelse(GENDER==1, "Male", "Female"),
         CVD_C = ifelse(CVD==1, "Yes", "No")) 


pubgraph2 <-
  ggplot(data = surv, mapping = aes(x =DIASBP , y =SYSBP )) + 
  geom_point() + 
  facet_grid(rows = vars(AFB_C), cols = vars(GENDER_C)) +
  labs(title="Systolic and Diasolic Blood Pressure by Gender and Disease Status",
       y="Systolic Blood Pressure",
       x="Diastolic Blood Pressure",
       caption="Data source: Survival 500 data")+
  geom_smooth(mapping=aes(group=NA,  x=DIASBP,y=SYSBP))+
  theme(panel.spacing = unit(0.5,"cm",data=NULL),
        strip.text.x=element_text(size=12, color="blue",face="bold.italic"),
        strip.text.y=element_text(size=12, color="blue",face="bold"),
        plot.title=element_text(face="italic", color="Blue"),
        panel.grid.minor=element_line(linetype="solid",color="black"),
        plot.background=element_rect(fill="lightblue",color="black")
        )

  ggplot2::ggsave(filename="pubgraph2.eps",
                  plot=pubgraph2,
                  device="eps",
                  dpi=1200,
                  width=15,
                  height=10)
  ggplot2::ggsave(filename="pubgraph2.jpg",
                  plot=pubgraph2,
                  device="jpg",
                  dpi=1200,
                  width=15,
                  height=10)',
dots1 = "Test4",
dotJA1 = "Journal Update",
dotJA2 = "Journal Update again..."
)

saveRDS(code_versions, file = "data/code_versions.rds")
library(shiny)
library(shinyAce)
library(shinyjs)
library(bslib)
library(purrr)
library(stringr)

read_and_assign <- function(file){
  data <- readRDS(file)
  assign(gsub("data/(.*?)\\.rds", "\\1", file), data, envir = .GlobalEnv)
}

list.files(path = "data/", full.names = TRUE) |> 
  map(read_and_assign)
list.files(path = "mods/", full.names = TRUE) |> 
  map(source)


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
                  height=10)',                          # initially Test1
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


pubgraph2<-
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
  dot3 = "### Exploring Other Analyses",              # Initially Test3
  dots1 = "Test4",
  dotJA1 = "Journal Update",
  dotJA2 = "Journal Update again..."
)
  
ui <-  fluidPage(
  includeCSS("www/style.css"),
  shinyjs::useShinyjs(),
  page_navbar(
    title = "Git in theory",
    theme = bs_theme(bootswatch ="minty"),
    nav_panel("Commiting",
              navset_card_tab(
                nav_panel("Q1",
                          commitq2_ui("q2")),
                nav_panel("Q2",  
                          commitq1_ui("q1")),
                nav_panel("Q3",  
                          commitq3_ui("q3"))
                
              )
              
    ),
    nav_panel("Making Branches",
              navset_card_tab(
                nav_panel("Q1",
                          branchq1_ui("bq1")),
                nav_panel("Q2",
                          branchq2_ui("bq2"))
              )
              ),
    nav_panel("Merging Branches", 
              navset_card_tab(
                nav_panel("Q1",
                          mergeq1_ui("mq1")),
                nav_panel("Q2",
                          mergeq2_ui("mq2")
                          )
              )
              )
  )
)


server <- function(input, output, session) {
  commitq1_server("q1")
  commitq2_server("q2")
  commitq3_server("q3")
  
  branchq1_server("bq1")
  branchq2_server("bq2")
  
  mergeq1_server("mq1")
  mergeq2_server("mq2")
}

shinyApp(ui = ui, server = server)

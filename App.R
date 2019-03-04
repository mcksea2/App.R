library(shiny)
library(tidyr)
library(dplyr)

av <- read.csv("allvar.csv")
avt <- mutate(av,time=visage-baseage)
str(avt)
avt$newpid <- as.factor(avt$newpid) #change the newpid column to be a factor for plotting
str(avt)

avt_fltr <-         
  avt %>%
  na.omit%>%
  group_by(newpid) %>%
  filter(n()>1) 


uiCD4 <- shinyUI(fluidPage(
  
  titlePanel("Patients in HIV treatment study"),
  
  #sidebarLayout with input/output definitions
  
  sidebarLayout(
    sidebarPanel(                              #Sidebar panel for inputs ----
      sliderInput(inputId = "n",                 #the number of individuals to plot 
            label = "Select patient", 
            min = 1, 
            max = 254, 
            value = 50,         #default
            step = 1)
    ),
    
    mainPanel(plotOutput("CD4_Plot"))            #Main panel for displaying outputs ----
  )))          


#Server actions            

serverCD4 <- shinyServer(function(input,output){
  # Output:  "CD4_Plot"   
  
  output$CD4_Plot <- renderPlot({   
    #avt_sample <- sample(avt_child_id,size=4,replace=TRUE) 
    avt_new <- filter(avt_fltr,(as.numeric(newpid)<=input$n))
    ggplot(avt_new, aes(x=time, y=sqrt(CD4PCT))) + 
      geom_point(aes(color=(newpid))) +  
      geom_smooth(method = "lm", se = FALSE) +
      theme(legend.position="none") + #does not include legend
      labs(title="Patients in HIV 2-year treatment study", 
           subtitle="Average CD4 across time for selected patient, where each patient is represented by a diff color", 
           y="% CD4 (square root)", 
           x="Time (years)",
           color=NULL)  # title and caption
  })
})   
# run shiny app
shinyApp(ui = uiCD4, server = serverCD4)     

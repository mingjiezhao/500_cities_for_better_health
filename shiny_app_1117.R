library(shiny)
library(tidyverse)

# data manipulation and cleaning
q3_data = read.csv("500_Cities__Local_Data_for_Better_Health__2018_release.csv", header=T, sep=",", stringsAsFactors =F)

q3_data$StateDesc = as.character(q3_data$StateDesc)
q3_data[which(q3_data$StateDesc == "North Carolin"),"StateDesc"] ="North Carolina"
q3_data[which(q3_data$StateDesc == "South Carolin"),"StateDesc"] ="South Carolina"


#shiny app
shinyApp(
  ui = fluidPage(
    headerPanel('The 500 Cities Project: Local data for better health'),
    # titlePanel(""),
    sidebarLayout(
      sidebarPanel(
     
        
          selectInput("measure_type", "Measure Category", 
                      choices=unique(dfl$Measure)),
          selectInput("State", "State", 
                      choices=unique(dfl$StateDesc)),
          hr()
        
    ),

    mainPanel(
      # h1(paste0("Results"), align = "center"),
      # h3("Plot of average values across the states"),
      plotOutput("phonePlot"),
      h3("Between state vs Within state"),
      textOutput("selected_var"),
      # dataTableOutput('table'),
      h3("Model Summary"),
      verbatimTextOutput("summary"),
      plotOutput("denPlot")
    ))
  ),
  server = function(input, output, session)
  {
    #plot 1: map plot
    output$phonePlot <- renderPlot({
      # data manipulation
      var = input$measure_type
      mental= q3_data[which(q3_data$Measure == var),]
      q_mr = mental %>%
        select(StateDesc, Data_Value) %>%
        group_by(StateDesc) %>%
        summarise(Average_value = mean(Data_Value, na.rm = T)) %>%
        arrange(desc(Average_value)) %>%
        as.data.frame()
      names(q_mr)[1] = "region"
      q_mr$region = tolower(q_mr$region)
      
      states <- map_data("state")
      map.df <- merge(states,q_mr, by="region", all=T)
      map.df <- map.df[order(map.df$order),]
      
      ggplot(map.df, aes(x=long,y=lat,group=group))+
        geom_polygon(aes(fill=Average_value))+
        geom_path()+ 
        scale_fill_gradientn(colours=rev(heat.colors(5)),na.value="grey90")+
        coord_map() +
        ggtitle(var) + 
        theme(plot.title = element_text(hjust = 0.5))+
        theme_void()
    })
    
    #plot 2: density plot comparison
    output$denPlot <- renderPlot({
      tot_select = q3_data[q3_data$StateDesc ==input$State, ]
      tot_select = tot_select %>% filter(Measure==input$measure_type | Measure=="Physical health not good for >=14 days among adults aged >=18 Years" | Measure=="Mental health not good for >=14 days among adults aged >=18 Years")
      
      tot_select2 = q3_data %>% filter(Measure==input$measure_type | Measure=="Physical health not good for >=14 days among adults aged >=18 Years" | Measure=="Mental health not good for >=14 days among adults aged >=18 Years")
      
      
      p3=  ggplot(tot_select, aes(Data_Value))+ geom_density(aes(fill=factor(Measure)), alpha=0.8) + 
        labs(title="Comparison of density for selected variable with mental and physical health", 
             subtitle=paste("Selected state:", input$State),
             
             x="Data Value",
             fill="Measures")
      
      p4 =  ggplot(tot_select2, aes(Data_Value))+ geom_density(aes(fill=factor(Measure)), alpha=0.8) + 
        labs(title="Comparison of density for selected variable with mental and physical health", 
             subtitle="Nationwide",
             
             x="Data Value",
             fill="Measures")
      grid.arrange(p3, p4)
    })
    output$selected_var <- renderText({ 
      dft = q3_data %>% filter(GeographicLevel=="City", DataValueTypeID=="AgeAdjPrv", Measure==input$measure_type) 
      m0_p = lmer(Data_Value~(1|StateDesc), REML=T, data=dft)
      table1 = as.data.frame(VarCorr(m0_p)) %>% select(grp, vcov, sdcor)
      paste("Based on the random intercept model, between-state variation accounts for ", round(100*table1[1,2]/(table1[1,2]+table1[2,2]),2), " % of total variance")
    })
    
    
    output$summary <- renderPrint({
      dft = q3_data %>% filter(GeographicLevel=="City", DataValueTypeID=="AgeAdjPrv", Measure==input$measure_type) 
      m0_p = lmer(Data_Value~(1|StateDesc), REML=T, data=dft)
      summary(m0_p)
    })
    



  }
)

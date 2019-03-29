#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(lubridate)
library(dplyr)
library(ggplot2)
library(markdown)
library(shinydashboard)
library(DT)
library(tidyverse)
library(rattle)
library(plotly)
library(shinyWidgets)
library(rsconnect)




google<-read.csv("new_R.csv")




# Define UI for application that draws a histogram
ui <- navbarPage(
      #inverse=TRUE,
      strong(span("Google Customer Revenue Analysis",style = "color:Green")),
      tabPanel("ReadMe",
               mainPanel(
                 #setBackgroundImage("LeftStackedBlueWeb_RGB.png"),
                 em(tags$b(h1("Google Customer Revenue Analysis"))),
                 
                 tags$div(strong(h3("Data Cleaning:")),
                 tags$li(h5("Delimiting JSON & Using visit start column for Date columns")),
                 tags$li(h5("Replacing the not set & not avaliable demoset values with the city considering the countries")),
                 tags$li(h5("Created Time Bins: Midnight,Morning,Afternoon,Evening")),
                 tags$li(h5("Subset the unwanted columns from data set"))),
                 
                 hr(),
                 
                 tags$div(strong(h3("Data Visulaizations:")),
                 tags$li(h5("Number of Visits by Continent")),
                 tags$li(h5("Number of PageViews by Continent")),
                 tags$li(h5("Number of Visits:Continent by Month")),
                 tags$li(h5("Number of Total Visits: Weekdays by Continent")),
                 tags$li(h5("Number of Visits: Top Countries by Continent")),
                 tags$li(h5("Number of Visits: Time of the day of top Countries by Continent")),
                 tags$li(h5("Number of visits: Time of the day of top Country Visits by Weekdays"))),
                 
                 hr(),
                 
                 tags$div(strong(h3("References:")),
                 tags$li(h5("Stack Overflow")),
                 tags$li(h5("Professor PK"))
                 )
               )),
      tabPanel(em("Year By Continent"),
               sidebarLayout(
                 sidebarPanel(

                   radioButtons("year", "Select Year", choices= unique(google$year),selected="2016")
                   #selectInput("Continent",em("Select Continent"),choices = unique(google$continent))
                 ),
                 
                 mainPanel(
                   tabsetPanel(
                    
                      # Data 
                     tabPanel(p(icon("table"), "Dataset"),
                              dataTableOutput("table")
                     ),# end of "Dataset" tab panel
                     
                     #plots
                     tabPanel(p(icon("line-chart"), "Visualize the Data"),
                              h4('Number of Visits by Continent', align = "center"),
                              h5('Please hover over each point to see the Continent and Total Number of Visits.', 
                                 align ="center"),
                              plotlyOutput("VisitsByContinent"),
                              
                              hr(),
                              
                              h4('Number of PageViews by Continent', align = "center"),
                              h5('Please hover over each point to see the Continent and Total Number of PageViews.', 
                                 align ="center"),
                              plotlyOutput("PageViewsByContinent")
                              ),# end of "Visualize the Data" tab panel
                     
                     #summary
                     tabPanel(p(icon("calculator"),"summary"),
                              verbatimTextOutput("summary")))# end of "Summary" tab panel
                 )#mainpanel
               )#sidebarlayout
      ),
      tabPanel(em("Google Visits by Continents "),
               sidebarLayout(
                 sidebarPanel(
                   #selectInput("Year",em(span("Select Year", style = "color:blue")),choices = unique(google$year)),
                   selectInput("year1",em("Select Year"),choices = unique(google$year)),
                   selectInput("continent",em("Select Continent"),choices = unique(google$continent))
                   #selectInput("country",em("Select Country"),choices = unique(google$country))
                  
                 ),
                 mainPanel(
                   tabsetPanel(
                     
                     # Data 
                     tabPanel(p(icon("table"), "Dataset"),
                              h3("Countries by Continent"),
                              hr(),
                              dataTableOutput("table1")
                              
                     ),# end of "Dataset" tab panel
                     
                     tabPanel(p(icon("line-chart"), "Visualize the Data"),
                              h4('Number of Total Visits: Weekdays by Continent', align = "center"),
                              h5('Please hover over each point to see the WeekDays and Total Number of Visits.', 
                                 align ="center"),
                              plotlyOutput("VisitsByWeekdays"),
                              
                              hr(),
                              h4('Number of Visits: Top Countries by Continent ', align = "center"),
                              h5('Please hover over each point to see the Country and Total Number of Visits.', 
                                  align ="center"),
                              plotlyOutput("VisitsByCountry")
                             
                            
                     ),# end of "Visualize the Data" tab panel
                     
                     #summary
                     tabPanel(p(icon("calculator"),"summary"),
                              verbatimTextOutput("summary1"),
                              hr(),
                              verbatimTextOutput("summary2")
                              )
                     )# end of "Summary" tab panel
                 )
                 )
               ),
      tabPanel(em("WeekDay Count"),
               sidebarLayout(
                 sidebarPanel(
                            selectInput("year2",em("Select Year"),choices = unique(google$year)),
                            selectInput("continent1",em("Select Continent"),choices = unique(google$continent)),
                            selectInput("month1",em("Select Month"),choices = sort(unique(google$month))),
                            # conditionalPanel(condition = "input.year2=='2016'",
                            #                  selectInput("month1",em("Select Month"),list("8","9","10","11","12"))
                            # ),
                            # conditionalPanel(condition = "input.year2=='2017'",
                            #                  selectInput("month1",em("Select Month"),list("1","2","3","4","5","6","7"))                           
                            # ),
                            radioButtons("Time",em("Select Time"),list("Midnight","Morning","Afternoon","Evening"))
                              ),
      mainPanel((
        tabsetPanel(
          tabPanel(p(icon("table"), "Dataset"),
                   hr(),
                   h3("Time of day visits by each Continent and country "),
                   hr(),
                   dataTableOutput("table2"),
                   hr(),
                   h3("Daily Count By Top Country Of Continent"),
                   hr(),
                   dataTableOutput("table2.1")
                   ),#end of Tab Panel
          
          tabPanel(
            p(icon("line-chart"), "Visualize the Data"),
            h4('Number of Visits: Time of the day of top Countries by Continent', align = "center"),
            h5('Please hover over each point to see the Country and Total Number of Visits.', 
               align ="center"),
            plotlyOutput("VisitsByTime"),
            
            hr(),
            
            h4('Number of visits: Time of the day of top Country Visits by Weekdays', align = "center"),
            h5('Please hover over each point to see the Continent and Total Number of Visits.', 
               align ="center"),
            plotlyOutput("VisitsByTimeWeekday")
          )
        )
      ))
               )
      )#End of Third Tab Panel
)
      

      




# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
    # output$ui_select_year <- renderUI({
    #   selectInput("si_year", "year", google$year)
    # })
    # 
    # output$ui_select_continet <- renderUI({
    #   selectInput("si_continent", "continent", google$continent)
    # })
    # 
    # output$ui_select_country <- renderUI({
    #   selectInput("si_country", "country", input$continent)
    # })
  
    
    
    #############Tables###########
    
    #1st Tab
    output$table <- renderDataTable({
      google %>%
        filter(year == input$year)
    })
    
    
    #2nd Tab
    output$table1<-renderDataTable({
        google%>%
        group_by(year,continent,country)%>%
        filter(year==input$year1 & continent==input$continent)%>%
        summarise(count=sum(visits))%>%
        arrange(desc(count))
      
    })
    output$table1.1<-renderDataTable({
      google%>%
        group_by(year,continent,WeekDay.name)%>%
        filter(year==input$year1 & continent==input$continent)%>%
        summarise(count=sum(visits))%>%
        arrange(desc(count))
    })
  
  
    #3rd Tab  
    output$table2<-renderDataTable({
        if(input$Time=="Morning"){
        google%>%
        #select(year,continent,country,Midnight,Morning,Afternoon,Evening)%>%
        group_by(year,continent,month,country)%>%
        filter(year==input$year2 & continent==input$continent1 & month==input$month1 &visits==1) %>%
        summarise(count=sum(Morning))%>%
        arrange(desc(count))}
      else{
        if(input$Time=="Afternoon"){google%>%
            #select(year,continent,country,Midnight,Morning,Afternoon,Evening)%>%
            group_by(year,continent,month,country)%>%
            filter(year==input$year2 & continent==input$continent1 & month==input$month1 & visits==1) %>%
            summarise(count=sum(Afternoon))%>%
            arrange(desc(count))}
        else{
          if(input$Time=="Evening"){google%>%
              #select(year,continent,country,Midnight,Morning,Afternoon,Evening)%>%
              group_by(year,continent,month,country)%>%
              filter(year==input$year2 & continent==input$continent1  & month==input$month1 & visits==1) %>%
              summarise(count=sum(Evening))%>%
              arrange(desc(count))}
          else{google%>%
                #select(year,continent,country,Midnight,Morning,Afternoon,Evening)%>%
                group_by(year,continent,month,country)%>%
                filter(year==input$year2 & continent==input$continent1 & month==input$month1 & visits==1) %>%
                summarise(count=sum(Midnight))%>%
                arrange(desc(count))}
      }
    }
  }

)
    
    output$table2.1<-renderDataTable({
        if(input$Time=="Morning"){
          google%>%
            group_by(year,continent,WeekDay.name)%>%
            filter(year==input$year2 & continent==input$continent1 & month==input$month1 & visits==1)%>%
            summarise(count=sum(Morning))%>%
            arrange(desc(count))}
        else{
          if(input$Time=="Afternoon"){
            google%>%
              group_by(year,continent,WeekDay.name)%>%
              filter(year==input$year2 & continent==input$continent1 & month==input$month1  & visits==1)%>%
              summarise(count=sum(Afternoon))%>%
              arrange(desc(count))}
          else{
            if(input$Time=="Evening"){
              google%>%
                group_by(year,continent,WeekDay.name)%>%
                filter(year==input$year2 & continent==input$continent1 & month==input$month1 & visits==1)%>%
                summarise(count=sum(Evening))%>%
                arrange(desc(count))}
            else{
              google%>%
                group_by(year,continent,WeekDay.name)%>%
                filter(year==input$year2 & continent==input$continent1 & month==input$month1 & visits==1)%>%
                summarise(count=sum(Midnight))%>%
                arrange(desc(count))}}
        }
    })
    
    ##############summary###############
    
    output$summary <- renderPrint({
      summary(google %>%
                filter(year == input$year))
    })
    
    output$summary1 <- renderPrint({
      summary(google%>%
                group_by(year,continent,country)%>%
                filter(year==input$year1 & continent==input$continent)%>%
                summarise(count=sum(visits))%>%
                arrange(desc(count))
      )
    })
    
    output$summary2 <- renderPrint({
      summary(if(input$Time=="Morning"){
        google%>%
          #select(year,continent,country,Midnight,Morning,Afternoon,Evening)%>%
          group_by(year,continent,country)%>%
          filter(year==input$year1 & continent==input$continent) %>%
          summarise(count=sum(visits))%>%
          arrange(desc(count))}
        else{
          if(input$Time=="Afternoon"){google%>%
              #select(year,continent,country,Midnight,Morning,Afternoon,Evening)%>%
              group_by(year,continent,country)%>%
              filter(year==input$year1 & continent==input$continent) %>%
              summarise(count=sum(visits))%>%
              arrange(desc(count))}
          else{
            if(input$Time=="Evening"){google%>%
                #select(year,continent,country,Midnight,Morning,Afternoon,Evening)%>%
                group_by(year,continent,country)%>%
                filter(year==input$year1 & continent==input$continent) %>%
                summarise(count=sum(visits))%>%
                arrange(desc(count))}
            else{google%>%
                #select(year,continent,country,Midnight,Morning,Afternoon,Evening)%>%
                group_by(year,continent,country)%>%
                filter(year==input$year1 & continent==input$continent) %>%
                summarise(count=sum(visits))%>%
                arrange(desc(count))}
          }
        }
      )
    })
    
    
    
    ##Reactive functions
    
    #1st Tab
    dataTableByVisitsContinent <- reactive({
        google %>%
        filter(year == input$year)%>%
        group_by(year,continent)%>%
        #filter(year == input$year & continent == input$continent)%>%
        summarise(count=sum(visits))
    })
    
    dataTableByPageviewsContinent <- reactive({
      google %>%
        filter(year == input$year)%>%
        group_by(year,continent)%>%
        #filter(year == input$year & continent == input$continent)%>%
        summarise(count=sum(pageviews))
    })
    
    #2nd Tab
    dataTableByVisitsCountries<-reactive({
        google%>%
        group_by(year,continent,country)%>%
        filter(year==input$year1 & continent==input$continent)%>%
        summarise(count=sum(visits))%>%
        arrange(desc(count))%>%
        head(5)
        
    })
    
    
    dataTableByVisitsWeekdays<-reactive({
      google%>%
        group_by(year,continent,WeekDay.name)%>%
        filter(year==input$year1 & continent==input$continent)%>%
        summarise(count=sum(visits))%>%
        arrange(desc(count))
    })
    
    #3rd Tab
    dataTableByVisitsTime<-reactive({if(input$Time=="Morning"){
      google%>%
        #select(year,continent,country,Midnight,Morning,Afternoon,Evening)%>%
        group_by(year,continent,month,country)%>%
        filter(year==input$year2 & continent==input$continent1 & month==input$month1  & visits==1) %>%
        summarise(count=sum(Morning))%>%
        arrange(desc(count))%>%
        head(5)}
      else{
        if(input$Time=="Afternoon"){google%>%
            #select(year,continent,country,Midnight,Morning,Afternoon,Evening)%>%
            group_by(year,continent,month,country)%>%
            filter(year==input$year2 & continent==input$continent1 & month==input$month1  & visits==1) %>%
            summarise(count=sum(Afternoon))%>%
            arrange(desc(count))%>%
            head(5)}
        else{
          if(input$Time=="Evening"){google%>%
              #select(year,continent,country,Midnight,Morning,Afternoon,Evening)%>%
              group_by(year,continent,month,country)%>%
              filter(year==input$year2 & continent==input$continent1 & month==input$month1  & visits==1) %>%
              summarise(count=sum(Evening))%>%
              arrange(desc(count))%>%
              head(5)}
          else{google%>%
              #select(year,continent,country,Midnight,Morning,Afternoon,Evening)%>%
              group_by(year,continent,month,country)%>%
              filter(year==input$year2 & continent==input$continent1 & month==input$month1  & visits==1) %>%
              summarise(count=sum(Midnight))%>%
              arrange(desc(count))%>%
              head(5)}
        }
      }
    })
    
  
    
    dataTableByVisitsCountry<-reactive({
      if(input$Time=="Morning"){
        google%>%
        group_by(year,continent,month,WeekDay.name)%>%
        filter(year==input$year2 & continent==input$continent1 & month==input$month1 & visits==1)%>%
        summarise(count=sum(Morning))%>%
        arrange(desc(count))}
      else{
        if(input$Time=="Afternoon"){
            google%>%
            group_by(year,continent,month,WeekDay.name)%>%
            filter(year==input$year2 & continent==input$continent1 & month==input$month1 & visits==1)%>%
            summarise(count=sum(Afternoon))%>%
            arrange(desc(count))}
        else{
          if(input$Time=="Evening"){
              google%>%
              group_by(year,continent,month,WeekDay.name)%>%
              filter(year==input$year2 & continent==input$continent1 & month==input$month1 & visits==1)%>%
              summarise(count=sum(Evening))%>%
              arrange(desc(count))}
          else{
              google%>%
              group_by(year,continent,month,WeekDay.name)%>%
              filter(year==input$year2 & continent==input$continent1 & month==input$month1 & visits==1)%>%
              summarise(count=sum(Midnight))%>%
              arrange(desc(count))}}
      }
    })
    
    
    
    
    ############ggplots#############
    
    #1 tab
    output$VisitsByCountry<- renderPlotly(
      {R1 <- ggplot(dataTableByVisitsCountries(), aes(x = reorder(country,-count), y = count, fill = continent)) + geom_bar(stat = "Identity") + xlab("Countries") + ylab("Number of Visits") +
        theme_classic()
      ggplotly(R1)
      }
    )

    output$PageViewsByContinent<- renderPlotly(
      {R <- ggplot(dataTableByPageviewsContinent(), aes(x = reorder(continent,-count), y = count, fill = continent)) + geom_bar(stat = "Identity") + xlab("Continents") + ylab("Number of Visits") +
        theme_classic()
      ggplotly(R)
      })
   
    
    #2 tab
    output$VisitsByWeekdays<- renderPlotly(
      {R3 <- ggplot(dataTableByVisitsWeekdays(), aes(x = reorder(WeekDay.name,-count), y = count, fill = continent)) + geom_bar(stat = "Identity") + xlab("Week Days") + ylab("Number of Visits") +
        theme_classic()
      ggplotly(R3)
      }
    )
    
    
    output$VisitsByContinent<- renderPlotly(
      {R4 <- ggplot(dataTableByVisitsContinent(), aes(x = reorder(continent,-count), y = count, fill = continent)) + geom_bar(stat = "Identity") + xlab("Continents") + ylab("Number of Visits") +
        theme_classic()
      ggplotly(R4)
      }
    )
    
    #3rd Tab 
  
    output$VisitsByTime<-renderPlotly(
      {R5 <- ggplot(dataTableByVisitsTime(), aes(x = reorder(country,-count), y = count, fill = continent)) + geom_bar(stat = "Identity") + xlab("Countries") + ylab("Number of Visits") +
        theme_classic()
      ggplotly(R5)
      }
    )  
   

    output$VisitsByTimeWeekday<-renderPlotly(
      {R6 <- ggplot(dataTableByVisitsCountry(), aes(x = reorder(WeekDay.name,-count), y = count, fill = continent)) + geom_bar(stat = "Identity") + xlab("Week Days") + ylab("Number of Visits") +
        theme_classic()
      ggplotly(R6)
      }
    )

    
}

# Run the application 
shinyApp(ui = ui, server = server)


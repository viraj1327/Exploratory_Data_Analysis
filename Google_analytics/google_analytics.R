library(shiny)
library(DBI)
library(RMySQL)
library(shinydashboard)
library(highcharter)
library(dplyr)
library(ggplot2)
library(plotly)

# df<-read.csv("~/Desktop/all/Original_R.csv")
df<-read.csv("~/Desktop/all/Oringinal_R.csv")
# df<- read.csv("R.csv")
df$modified_date<-format(as.POSIXct(df$new_date,format='%Y-%m-%d %H:%M:%S'),format='%Y-%m-%d')
View(df)
df$modified_date<-as.Date(df$modified_date,format='%Y-%m-%d')

df['bounces'][is.na(df['bounces'])] <- 0
new_df<-df
  
ui<- dashboardPage(
  
  dashboardHeader(title = "Google Analytics"),
  
  dashboardSidebar(
    sidebarMenu(
      dateRangeInput("date", label = "Date range",start="2016-09-02",end = "2017-07-31",separator = '-',format = "yyyy-mm-dd"),
      #sliderInput("time",  "Date", min = ymd("2016-09-02"),  max = ymd("2017-01-05"),value = c(ymd("2016-09-02"),ymd("2017-01-05"))),
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Table", tabName = "table", icon = icon("table"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "dashboard",
        
        fluidRow(
          
              column(
                width=9,
                  
                fluidRow(
                  valueBoxOutput("Users",width=3),
                  valueBoxOutput("hitsperpageviewsBox",width=3),
                  valueBoxOutput("pageviewsBox",width=3),
                  valueBoxOutput("bounceBox",width=3)
                  ),
                
                fluidRow(
                  box(
                    width = 12,
                    # highchartOutput(
                    #   "VisitsByTimeWeekdays",
                    #   height = 500
                    # )
                    h2(strong('Distribution of Users By Week Days'), 
                       align ="center"),
                    h6(strong("Hover for the distribution of particular day "),align="center"),
                    plotlyOutput("VisitsByTimeWeekdays",
                                  height=500)
                  )
                )
                
              ),#end of column
              box(
                width = 3,
                title = "Data Filter",
                selectInput("continent",em("Select Continent"),choices = unique(df$continent)),
                uiOutput("countryselection"),
                radioButtons("time",em("Time of the day"),list("Morning","Afternoon","Evening","Midnight"),selected = "Morning")
                
              )
              
            )
        

      ),#end of first tab
      
    #second tab
    tabItem(tabName = "table",
            box(width=18,
                dataTableOutput("table")),
            downloadButton("download",strong("Download Data"))
            )#end of second tab
    
    )
    
  )#end of dashboardbody
  
)#end of dashboardpage

      


server <- function(input, output,session) {
 
  
  
  output$countryselection<-renderUI({
    
    available <- df[df$continent == input$continent, "country"]
    
    
    selectInput(
      inputId = "country", 
      label = em("Select Country"),
      choices = sort(unique(available)),
      selected = unique(available)[1])
    
  })
  
  
  table<-reactive({df%>%
    filter(modified_date>=input$date[1] & modified_date<=input$date[2])%>%
    select(hits,bounces,pageviews,visits,continent,country,city,year,month,Day,WeekDay.name)%>%
    filter(continent==input$continent & country==input$country)})
  
  
  output$table<-renderDataTable({
        table()
      })
  
  
    
    
  output$Users<-renderValueBox({
    out_users<-df%>%
      filter(modified_date>=input$date[1] & modified_date<=input$date[2])%>%
      select(hits,bounces,pageviews,visits,continent,country)%>%
      filter(continent==input$continent & country==input$country)%>%
      summarise(sum(visits))
    
    valueBox(
      paste0(out_users),"Users", icon = icon("user", lib = "glyphicon"),
      color = "blue")
  })
  
  
  
  output$hitsperpageviewsBox<- renderValueBox({
      out_hitsperpage<-df%>%
        filter(modified_date>=input$date[1] & modified_date<=input$date[2])%>%
        select(hits,bounces,pageviews,visits,continent,country)%>%
        filter(continent==input$continent & country==input$country)%>%
        summarise(round((sum(hits)/sum(pageviews)),2))
      
      valueBox(paste0(out_hitsperpage,"%"),"HitsperPageviews", icon = icon("thumbs-up", lib = "glyphicon"),
               color = "light-blue")
      
    })
  
  
  output$pageviewsBox <- renderValueBox({
    out<- df%>%
            filter(modified_date>=input$date[1] & modified_date<=input$date[2])%>%
            select(hits,bounces,pageviews,visits,continent,country)%>%
            filter(continent==input$continent & country==input$country)%>%
            summarise(sum(pageviews))
    valueBox(
       out,"Pageviews", icon = icon("list"),
      color = "purple")
  })
#paste0(25 + input$count, "%"),
  

  
  output$bounceBox <- renderValueBox({
    out_bouncerate<-df%>%
      filter(df$modified_date>=input$date[1] & df$modified_date<=input$date[2])%>%
      select(hits,bounces,pageviews,visits,continent,country)%>%
      filter(continent==input$continent & country==input$country)%>%
      summarise(round((sum(bounces)/sum(new_df$bounces)),2))
    valueBox(
      paste0(out_bouncerate,"%"),"bounce rate", icon = icon("thumbs-down", lib = "glyphicon"),
      color = "yellow")
  })
  
#paste0(out_bouncerate,"%")
  
  dataTableByVisitsCountry<-reactive({
    if(input$time=="Morning"){
        df%>%
        filter(df$modified_date>=input$date[1] & df$modified_date<=input$date[2])%>%
        select(continent,country,WeekDay.name,Morning)%>%
        group_by(continent,WeekDay.name)%>%
        filter(continent==input$continent & country==input$country)%>%
        summarise(count=sum(Morning))%>%
        arrange(desc(count))}
    else{
      if(input$time=="Afternoon"){
          df%>%
          filter(df$modified_date>=input$date[1] & df$modified_date<=input$date[2])%>%
          select(continent,country,WeekDay.name,Afternoon)%>%
          group_by(continent,WeekDay.name)%>%
          filter(continent==input$continent & country==input$country)%>%
          summarise(count=sum(Afternoon))%>%
          arrange(desc(count))}
      else{
        if(input$time=="Evening"){
            df%>%
            filter(df$modified_date>=input$date[1] & df$modified_date<=input$date[2])%>%
            select(continent,country,WeekDay.name,Evening)%>%
            group_by(continent,WeekDay.name)%>%
            filter(continent==input$continent & country==input$country)%>%
            summarise(count=sum(Evening))%>%
            arrange(desc(count))}
        else{
            df%>%
            filter(df$modified_date>=input$date[1] & df$modified_date<=input$date[2])%>%
            select(continent,country,WeekDay.name,Midnight)%>%
            group_by(continent,WeekDay.name)%>%
            filter( continent==input$continent & country==input$country)%>%
            summarise(count=sum(Midnight))%>%
            arrange(desc(count))}}
    }
  })

  output$VisitsByTimeWeekdays<-renderPlotly(
    {R6 <- ggplot(dataTableByVisitsCountry(), aes(x = reorder(WeekDay.name,-count), y = count, fill = continent)) + geom_bar(stat = "Identity") + xlab("Week Days") + ylab("Number of Users/Visits") +
      theme_classic()
    ggplotly(R6)
    }
  )
  
  output$download <- downloadHandler(
    filename = function() { paste(input$continet,input$country,df$modified_date>=input$date[1] & df$modified_date<=input$date[2],sep = "") },
    content = function(file) {
      write.csv(table(), file)
       })
  
}
#Highchart
#reorder(WeekDay.name,-count)

# Create a Shiny app object
shinyApp( ui,  server)








# subset(df,select=c('hits','bounces','pageviews','visits','continent','country','city','year','month','Day'),df$modified_date>=input$date[1] & df$modified_date<=input$date[2])%>%

#out_users<-subset(df,select=c('hits','bounces','pageviews','visits','continent','country','city','year','month','Day'),df$modified_date>=input$date[1] & df$modified_date<=input$date[2])%>%
# filter(df$continent==input$continent)%>%
#   summarise(sum(visits))



# google_sample<-read.csv("~/Desktop/all/R.csv")
# google_sample1<-read.csv("~/Desktop/all/new_edited_train.csv")

# subsetting data with respective dates
# filtered_data <- reactive({filter(df, Date >= input$time[1], Date <= input$time[2])})

# filtered_data <- observe({
#   updateDateRangeInput(session, "date", min = "2016-09-02", max = "2017-01-05")
# })

# filtered_data <- reactive({
#   s<- subset(df,df$modified_date>=input$date[1],df$modified_date<=input$date[2])
#   dataTableOutput(s$hits)
#   # filter(df, between(date ,input$date[1], input$date[2]))
#   # updateDateRangeInput(session, "date", min = "2016-09-02", max = "2017-01-05")
# })

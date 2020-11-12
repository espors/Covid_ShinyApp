## import datasets ##-------------------------------------------------------------
library(readr)
Census <- read_csv("Census.csv")
covid_nov <- read_csv("covid_nov.csv", col_types = cols(submission_date = col_date(format = "%m/%d/%Y")))
csvData <- read_csv("csvData.csv")


## load necessary libraries ##---------------------------------------------------
library(tidyverse)
library(ggplot2)
library(zoo)
library(plotly)


##wrangle data##-----------------------------------------------------------------
##joing census and csv to get state codes 
colnames(csvData)[colnames(csvData) == "State"] <- "NAME"
Census <- inner_join(Census, csvData, by = "NAME")

##join census and covid, remove unnecessary variables 
colnames(Census)[colnames(Census) == "Code"] <- "state"
covid <- inner_join(covid_nov, Census, by = "state")

covid <- select(covid, -c(prob_cases,pnew_case, prob_death, 
                          pnew_death, created_at, consent_cases, 
                          consent_deaths, SUMLEV, DIVISION, STATE, 
                          Abbrev))

#calculate adj cases and deaths and rolling averages 
covid <- covid %>%
    group_by(state) %>%
    mutate(new_case_adj = new_case/POPESTIMATE2019 * 100000) %>%
    mutate(new_death_adj = new_death/POPESTIMATE2019 * 100000) %>%
    mutate(case5 = rollmean(new_case_adj, k =7, fill = NA)) %>%
    mutate(death5 = rollmean(new_death_adj, k=7, fill = NA)) %>%
    ungroup()


library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Covid-19 in the United States"),

    # Sidebar 
    sidebarLayout(
        
        
        sidebarPanel(
            
          #allow user to select region 
            selectInput("select_region", label = h3("Select US Region"), 
                        choices = list("Northeast" = 1, "Midwest" = 2, "South" = 3, "West" = 4), 
                        selected = 1),
          
          #allow user to select cases or deaths  
            selectInput("select_type", label = h3("Select for Number of Cases or Deaths"), 
                        choices = list("Cases" = "Covid Cases", "Deaths" = "Covid Deaths")),
        ),

        # show time series plot 
        mainPanel(
           plotlyOutput("timeSeries"),
        )
    )
)

# Define server logic 
server <- function(input, output) {
  
    output$timeSeries <- renderPlotly({
      
        #filter on region selected and dates before march 3 
        covid_region <- covid %>%
            filter(REGION == input$select_region) %>%
            filter(submission_date >= "2020-03-01")
        
        
        #graph for cases 
        plot <-ggplot(covid_region, aes(x = submission_date, group = state)) + 
            geom_line(aes(y = case5, color = state), size = 1) + 
            ylab("Rolling 5 Day Average Per 100,000") + 
            xlab("Date") + 
            ggtitle(input$select_type) + 
            scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") +
            theme_minimal() 
        
        #graph for deaths 
        plot2 <-ggplot(covid_region, aes(x = submission_date, group = state)) + 
            geom_line(aes(y = death5, color = state), size = 1) + 
            ylab("Rolling 5 Day Average Per 100,000") + 
            xlab("Date") + 
            ggtitle(input$select_type) + 
            theme(axis.text.x = element_text(angle = 90)) +
            theme_minimal() 
        
        #display plot based on user selection 
        if(input$select_type == "Covid Cases") {
            plot.ly <- ggplotly(plot)
        }
        
        else {
            plot.ly <- ggplotly(plot2)
        }
        
        
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

library(shiny)
library(shinydashboard)
library(ggplot2)


covid_data = read.csv('./Data/covid_19_india.csv', stringsAsFactors = FALSE)
covid_data$Date <- as.Date(covid_data$Date, "%d/%m/%y")

class(covid_data$Date)
head(covid_data$Date, 5)



# Get data

getData <- function(para)
{
  udates <- unique(covid_data$Date)
  totalCases <- c()
  
  for (d in udates) {
    temp1 <- covid_data[which(covid_data$Date == d),]
    
    totalCases <- c(totalCases, sum(temp1[[para]]))
  }
  
  tail(totalCases, 1)
}


totalConfirmedCases = getData("Confirmed")
totalCuredCases = getData("Cured")
totalDeathCases = getData("Deaths")


# function to plot graph for national data
plotNationalGraph <- function(ydata)
{
  uniqueDate <- unique(covid_data$Date)
  
  cases <- c()
  
  for (d in uniqueDate) {
    temp1 <- covid_data[which(covid_data$Date == d),]
    
    cases <- c(cases, sum(temp1[[ydata]]))
  }

  # data_df <- data.frame("Date" = uniqueDate, "Cases" = cases)
  # write.csv(data_df, "C:\\Users\\asus\\Desktop\\R\\covid19-in-india\\cases.csv")
  
  tempData <- data.frame("Date" = uniqueDate, "cases" = cases)
  
  name <- paste("National", ydata, sep = " ")
  
  p <- ggplot(data = tempData, aes(x = Date, y = cases)) +
        geom_line(color = "orange")+
        geom_point(colour = "red", size = 3) + 
        labs(x = "Date", y = ydata, title = name) +
        theme(axis.text=element_text(size=14), axis.title=element_text(size=16)) +
        theme(plot.title = element_text(size=22, color='purple', face = "bold"))
  
        
  
  plot(p)  
}


# for state wise data

plotStateGraph <- function(ydata, stateName)
{
  state_data = covid_data[which(covid_data$State.UnionTerritory == stateName),] 
  
  
  state_data_df = data.frame("Date" = state_data$Date, "cases" = state_data[[ydata]])
  name <- paste(stateName, ydata, sep = " ")
  
  p <- ggplot(data = state_data_df, aes(x = Date, y = cases)) +
    geom_line(color = "orange")+
    geom_point(colour = "red", size = 3) +
    labs(x = "Date", y = ydata, title = name) +
    theme(axis.text=element_text(size=14), axis.title=element_text(size=16)) +
    theme(plot.title = element_text(size=22, color='purple', face = "bold"))
  
  plot(p)
}

# states list
stateNames <- unique(covid_data$State.UnionTerritory)



ui <- dashboardPage(
  skin = "purple",
  title = "Corona Virus Data",
  dashboardHeader(title = "Coronavirus India Cases"),
  dashboardSidebar(
      sidebarMenu(
        menuItem("National", tabName = "national", icon = icon("tree")),
        menuItem("State", tabName = "state", icon = icon("car"))
      )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        "national",
        fluidRow(
          column(
            width = 10,
            box(plotOutput("confirmed_cases"), width = 10,),
            box(
              width = 2,
              h3(paste("Total : ", totalConfirmedCases)),
              h3(paste("Cured : ", totalCuredCases)),
              h3(paste("Death : ", totalDeathCases))
            ),
          )

        ),
        fluidRow(
          column(
            width = 10,
            box(plotOutput("cured_cases"), width = 5),
            box(plotOutput("death_cases"), width = 5)
          ),
        )
      ),
      tabItem(
        "state",
        fluidRow(
          column(
            width = 10,
            box(plotOutput("state_confirmed_cases"), width = 10,),
            box(
              width = 2,
              selectInput(
                "state", "State:", stateNames,
              )
            ),
          ),
        ),
        fluidRow(
          column(
            width = 10,
            box(plotOutput("state_cured_cases"), width = 5),
            box(plotOutput("state_death_cases"), width = 5)
          ),
        ),
      )
    )
  )
)

server <- function(input, output)
{
  output$confirmed_cases <- renderPlot({
    plotNationalGraph("Confirmed")
  })
  output$cured_cases <- renderPlot({
    plotNationalGraph("Cured")
  })
  output$death_cases <- renderPlot({
    plotNationalGraph("Deaths")
  })
  
  output$state_confirmed_cases <- renderPlot({
    plotStateGraph("Confirmed", input$state)
  })
  output$state_cured_cases <- renderPlot({
    plotStateGraph("Cured", input$state)
  })
  output$state_death_cases <- renderPlot({
    plotStateGraph("Deaths", input$state)
  })
}


shinyApp(ui, server)


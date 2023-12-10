library(shiny)
library(dplyr)
library(ggplot2)
library(fmsb)
library(plotly)
library(tidyr)  
library(leaflet)

df <- read.csv("Seattle_Neighborhood_Demographics.csv")


beacon_2020 <- c(pull(filter(df, neighborhood == "Beacon Hill"), total_pop_2020), 
                 pull(filter(df, neighborhood == "Beacon Hill"), white_alone_2020),
                 pull(filter(df, neighborhood == "Beacon Hill"), black_alone_2020), 
                 pull(filter(df, neighborhood == "Beacon Hill"), asian_alone_2020), 
                 pull(filter(df, neighborhood == "Beacon Hill"), am_native_alone_2020),
                 pull(filter(df, neighborhood == "Beacon Hill"), pi_alone_2020),
                 pull(filter(df, neighborhood == "Beacon Hill"), hispanic_alone_2020),
                 pull(filter(df, neighborhood == "Beacon Hill"), other_alone_2020),
                 pull(filter(df, neighborhood == "Beacon Hill"), two_more_2020))

beacon_2010 <- c(pull(filter(df, neighborhood == "Beacon Hill"), total_pop_2010), 
                 pull(filter(df, neighborhood == "Beacon Hill"), white_alone_2010),
                 pull(filter(df, neighborhood == "Beacon Hill"), black_alone_2010), 
                 pull(filter(df, neighborhood == "Beacon Hill"), asian_alone_2010), 
                 pull(filter(df, neighborhood == "Beacon Hill"), am_native_alone_2010),
                 pull(filter(df, neighborhood == "Beacon Hill"), pi_alone_2010),
                 pull(filter(df, neighborhood == "Beacon Hill"), hispanic_alone_2010),
                 pull(filter(df, neighborhood == "Beacon Hill"), other_alone_2010),
                 pull(filter(df, neighborhood == "Beacon Hill"), two_more_2010)) 

beacon_df <- data.frame(beacon_2010, beacon_2020)

beacon_df$demographic <- 3

beacon_df[beacon_df$beacon_2010 == 8022, "demographic"] <- "Total"
beacon_df[beacon_df$beacon_2010 == 1711, "demographic"] <- "White"
beacon_df[beacon_df$beacon_2010 == 922, "demographic"] <- "Black"
beacon_df[beacon_df$beacon_2010 == 4389, "demographic"] <- "Asian"
beacon_df[beacon_df$beacon_2010 == 37, "demographic"] <- "Native"
beacon_df[beacon_df$beacon_2010 == 93, "demographic"] <- "Pacific Islander"
beacon_df[beacon_df$beacon_2010 == 484, "demographic"] <- "Hispanic"
beacon_df[beacon_df$beacon_2010 == 19, "demographic"] <- "Other"
beacon_df[beacon_df$beacon_2010 == 498, "demographic"] <- "Two or More"

beacon_df <- beacon_df %>% gather("year", "population", beacon_2010, beacon_2020)

beacon_df[beacon_df$year == "beacon_2010", "year"] <- "2010"
beacon_df[beacon_df$year == "beacon_2020", "year"] <- "2020"

beacon_change_df <- data.frame(beacon_2010, beacon_2020)

beacon_change_df$demographic <- 3

beacon_change_df[beacon_change_df$beacon_2010 == 8022, "demographic"] <- "Total"
beacon_change_df[beacon_change_df$beacon_2010 == 1711, "demographic"] <- "White"
beacon_change_df[beacon_change_df$beacon_2010 == 922, "demographic"] <- "Black"
beacon_change_df[beacon_change_df$beacon_2010 == 4389, "demographic"] <- "Asian"
beacon_change_df[beacon_change_df$beacon_2010 == 37, "demographic"] <- "Native"
beacon_change_df[beacon_change_df$beacon_2010 == 93, "demographic"] <- "Pacific Islander"
beacon_change_df[beacon_change_df$beacon_2010 == 484, "demographic"] <- "Hispanic"
beacon_change_df[beacon_change_df$beacon_2010 == 19, "demographic"] <- "Other"
beacon_change_df[beacon_change_df$beacon_2010 == 498, "demographic"] <- "Two or More"

beacon_change_df$perc_change <- 0.0

beacon_change_df[beacon_change_df$demographic == "Total", "perc_change"] <- round(((9611 - 8022) / 8022) * 100, 2)
beacon_change_df[beacon_change_df$demographic == "White", "perc_change"] <- round(((2425 - 1711) / 1711) * 100, 2)
beacon_change_df[beacon_change_df$demographic == "Black", "perc_change"] <- round(((764 - 922) / 922) * 100, 2)
beacon_change_df[beacon_change_df$demographic == "Asian", "perc_change"] <- round(((4791 - 4389) / 4389) * 100, 2)
beacon_change_df[beacon_change_df$demographic == "Native", "perc_change"] <- round(((31 - 37) / 37) * 100, 2)
beacon_change_df[beacon_change_df$demographic == "Pacific Islander", "perc_change"] <- round(((29 - 93) / 93) * 100, 2)
beacon_change_df[beacon_change_df$demographic == "Hispanic", "perc_change"] <- round(((890 - 484) / 484) * 100, 2)
beacon_change_df[beacon_change_df$demographic == "Other", "perc_change"] <- round(((48 - 19) / 19) * 100, 2)
beacon_change_df[beacon_change_df$demographic == "Two or More", "perc_change"] <- round(((902 - 498) / 498) * 100, 2)


central_2020 <- c(pull(filter(df, neighborhood == "Central District"), total_pop_2020), 
                  pull(filter(df, neighborhood == "Central District"), white_alone_2020),
                  pull(filter(df, neighborhood == "Central District"), black_alone_2020), 
                  pull(filter(df, neighborhood == "Central District"), asian_alone_2020), 
                  pull(filter(df, neighborhood == "Central District"), am_native_alone_2020),
                  pull(filter(df, neighborhood == "Central District"), pi_alone_2020),
                  pull(filter(df, neighborhood == "Central District"), hispanic_alone_2020),
                  pull(filter(df, neighborhood == "Central District"), other_alone_2020),
                  pull(filter(df, neighborhood == "Central District"), two_more_2020))

central_2010 <- c(pull(filter(df, neighborhood == "Central District"), total_pop_2010), 
                  pull(filter(df, neighborhood == "Central District"), white_alone_2010),
                  pull(filter(df, neighborhood == "Central District"), black_alone_2010), 
                  pull(filter(df, neighborhood == "Central District"), asian_alone_2010), 
                  pull(filter(df, neighborhood == "Central District"), am_native_alone_2010),
                  pull(filter(df, neighborhood == "Central District"), pi_alone_2010),
                  pull(filter(df, neighborhood == "Central District"), hispanic_alone_2010),
                  pull(filter(df, neighborhood == "Central District"), other_alone_2010),
                  pull(filter(df, neighborhood == "Central District"), two_more_2010)) 

central_df <- data.frame(central_2010, central_2020)

central_df$demographic <- 3

central_df[central_df$central_2010 == 11296, "demographic"] <- "Total"
central_df[central_df$central_2010 == 6228, "demographic"] <- "White"
central_df[central_df$central_2010 == 2737, "demographic"] <- "Black"
central_df[central_df$central_2010 == 729, "demographic"] <- "Asian"
central_df[central_df$central_2010 == 53, "demographic"] <- "Native"
central_df[central_df$central_2010 == 78, "demographic"] <- "Pacific Islander"
central_df[central_df$central_2010 == 893, "demographic"] <- "Hispanic"
central_df[central_df$central_2010 == 24, "demographic"] <- "Other"
central_df[central_df$central_2010 == 722, "demographic"] <- "Two or More"

central_df <- central_df %>% gather("year", "population", central_2010, central_2020)

central_df[central_df$year == "central_2010", "year"] <- "2010"
central_df[central_df$year == "central_2020", "year"] <- "2020"

central_change_df <- data.frame(central_2010, central_2020)

central_change_df$demographic <- 3

central_change_df[central_change_df$central_2010 == 11296, "demographic"] <- "Total"
central_change_df[central_change_df$central_2010 == 6228, "demographic"] <- "White"
central_change_df[central_change_df$central_2010 == 2737, "demographic"] <- "Black"
central_change_df[central_change_df$central_2010 == 729, "demographic"] <- "Asian"
central_change_df[central_change_df$central_2010 == 53, "demographic"] <- "Native"
central_change_df[central_change_df$central_2010 == 78, "demographic"] <- "Pacific Islander"
central_change_df[central_change_df$central_2010 == 893, "demographic"] <- "Hispanic"
central_change_df[central_change_df$central_2010 == 24, "demographic"] <- "Other"
central_change_df[central_change_df$central_2010 == 722, "demographic"] <- "Two or More"

central_change_df$perc_change <- 0.0

central_change_df[central_change_df$demographic == "Total", "perc_change"] <- round(((19703 - 11296) / 11296) * 100, 2)
central_change_df[central_change_df$demographic == "White", "perc_change"] <- round(((11557 - 6228) / 6228) * 100, 2)
central_change_df[central_change_df$demographic == "Black", "perc_change"] <- round(((2396 - 2737) / 2737) * 100, 2)
central_change_df[central_change_df$demographic == "Asian", "perc_change"] <- round(((2155 - 729) / 729) * 100, 2)
central_change_df[central_change_df$demographic == "Native", "perc_change"] <- round(((70 - 53) / 53) * 100, 2)
central_change_df[central_change_df$demographic == "Pacific Islander", "perc_change"] <- round(((52 - 78) / 78) * 100, 2)
central_change_df[central_change_df$demographic == "Hispanic", "perc_change"] <- round(((1746 - 893) / 893) * 100, 2)
central_change_df[central_change_df$demographic == "Other", "perc_change"] <- round(((108 - 24) / 24) * 100, 2)
central_change_df[central_change_df$demographic == "Two or More", "perc_change"] <- round(((2345 - 722) / 722) * 100, 2)

international_2020 <- c(pull(filter(df, neighborhood == "International District"), total_pop_2020), 
                        pull(filter(df, neighborhood == "International District"), white_alone_2020),
                        pull(filter(df, neighborhood == "International District"), black_alone_2020), 
                        pull(filter(df, neighborhood == "International District"), asian_alone_2020), 
                        pull(filter(df, neighborhood == "International District"), am_native_alone_2020),
                        pull(filter(df, neighborhood == "International District"), pi_alone_2020),
                        pull(filter(df, neighborhood == "International District"), hispanic_alone_2020),
                        pull(filter(df, neighborhood == "International District"), other_alone_2020),
                        pull(filter(df, neighborhood == "International District"), two_more_2020))

international_2010 <- c(pull(filter(df, neighborhood == "International District"), total_pop_2010), 
                        pull(filter(df, neighborhood == "International District"), white_alone_2010),
                        pull(filter(df, neighborhood == "International District"), black_alone_2010), 
                        pull(filter(df, neighborhood == "International District"), asian_alone_2010), 
                        pull(filter(df, neighborhood == "International District"), am_native_alone_2010),
                        pull(filter(df, neighborhood == "International District"), pi_alone_2010),
                        pull(filter(df, neighborhood == "International District"), hispanic_alone_2010),
                        pull(filter(df, neighborhood == "International District"), other_alone_2010),
                        pull(filter(df, neighborhood == "International District"), two_more_2010)) 

international_df <- data.frame(international_2010, international_2020)

international_df$demographic <- 3

international_df[international_df$international_2010 == 3860, "demographic"] <- "Total"
international_df[international_df$international_2010 == 1122, "demographic"] <- "White"
international_df[international_df$international_2010 == 558, "demographic"] <- "Black"
international_df[international_df$international_2010 == 1769, "demographic"] <- "Asian"
international_df[international_df$international_2010 == 37, "demographic"] <- "Native"
international_df[international_df$international_2010 == 46, "demographic"] <- "Pacific Islander"
international_df[international_df$international_2010 == 206, "demographic"] <- "Hispanic"
international_df[international_df$international_2010 == 9, "demographic"] <- "Other"
international_df[international_df$international_2010 == 191, "demographic"] <- "Two or More"

international_df <- international_df %>% gather("year", "population", international_2010, international_2020)

international_df[international_df$year == "international_2010", "year"] <- "2010"
international_df[international_df$year == "international_2020", "year"] <- "2020"

international_change_df <- data.frame(international_2010, international_2020)

international_change_df$demographic <- 3

international_change_df[international_change_df$international_2010 == 3860, "demographic"] <- "Total"
international_change_df[international_change_df$international_2010 == 1122, "demographic"] <- "White"
international_change_df[international_change_df$international_2010 == 558, "demographic"] <- "Black"
international_change_df[international_change_df$international_2010 == 1769, "demographic"] <- "Asian"
international_change_df[international_change_df$international_2010 == 37, "demographic"] <- "Native"
international_change_df[international_change_df$international_2010 == 46, "demographic"] <- "Pacific Islander"
international_change_df[international_change_df$international_2010 == 206, "demographic"] <- "Hispanic"
international_change_df[international_change_df$international_2010 == 9, "demographic"] <- "Other"
international_change_df[international_change_df$international_2010 == 191, "demographic"] <- "Two or More"

international_change_df$perc_change <- 0.0

international_change_df[international_change_df$demographic == "Total", "perc_change"] <- round(((6309 - 3860) / 3860) * 100, 2)
international_change_df[international_change_df$demographic == "White", "perc_change"] <- round(((1970 - 1122) / 1122) * 100, 2)
international_change_df[international_change_df$demographic == "Black", "perc_change"] <- round(((739 - 558) / 558) * 100, 2)
international_change_df[international_change_df$demographic == "Asian", "perc_change"] <- round(((2516 - 1769) / 1769) * 100, 2)
international_change_df[international_change_df$demographic == "Native", "perc_change"] <- round(((85 - 37) / 37) * 100, 2)
international_change_df[international_change_df$demographic == "Pacific Islander", "perc_change"] <- round(((28 - 46) / 46) * 100, 2)
international_change_df[international_change_df$demographic == "Hispanic", "perc_change"] <- round(((552 - 206) / 206) * 100, 2)
international_change_df[international_change_df$demographic == "Other", "perc_change"] <- round(((39 - 9) / 9) * 100, 2)
international_change_df[international_change_df$demographic == "Two or More", "perc_change"] <- round(((563 - 191) / 191) * 100, 2)

story_view <- fluidPage(
  h2("The Story"),
  h3("Overview"),
  p("While people of all backgrounds make Seattle a cherished place, the city still has a problem. 
    From people's ethnicity, class, cultural behavior, and customs, Seattle is where the diverse population 
    can appreciate each other's heritage. However, the issue lies with Seattle modernizing its infrastructure. 
    Certain groups of people of color have been struggling to keep up with these changes. Thus, being displaced
    in their own neighborhoods. After comparing recent and past statistics, it is clear that Seattle is 
    undergoing an epidemic of gentrification."),
  p("What results from having multiple ethnic groups in a large city is that people with similar 
    backgrounds will live close to one another. Therefore, different parts of Seattle will naturally 
    have neighborhoods dominating a particular group. Neighborhoods such as Central District, International 
    District, and Beacon Hill will have varying demographics in racial/ethnic groups. "),
  p("The term gentrification was coined in the 1960s by British sociologist Ruth Glass to describe the displacement
    of working-class residents of London by middle-class newcomers. The definition, as said by Oxford Languages, 
    is 'the process whereby the character of a poor urban area is changed by wealthier people moving in, 
    improving housing, and attracting new businesses, typically displacing current inhabitants in the process.' 
    Therefore, gentrification has been around for a while. However, the growing statistics that follow will 
    continue to be alarming."),
  p("Seattle is constantly modernizing its infrastructure, including the neighborhoods that have housed
    people of color for generations. However, the change is not something to brush off. Housing prices in 
    America, especially Seattle, have been steadily increasing over the past few years. According to the 
    Seattle Times, the median sale price of homes in King County in September of 2023 was over $900k. 
    Mortgage rates have hit a 23-year high, affecting low income residents of neighborhoods that are being 
    gentrified. The residents of these neighborhoods are being displaced, causing the cost of housing to 
    increase at an alarming rate."),
  p("Although gentrification has been around for a long time, it should not be normalized. Choosing to 
    look the other way with Seattle's gentrification is equivalent to fanning the fire. Staying vocal 
    is how to avoid ignorance concerning this epidemic. By doing so, statistics become a voice for people 
    of color."),
)

analysis_view <- fluidPage(
  titlePanel("Neighborhood Demographic Analysis"), 
  tabsetPanel(
    tabPanel(
      "Beacon Hill",
      h3("About the neighborhood:"),
      p("Among the three neighborhoods shown, in 2010, Beacon Hill had the highest population of people of 
        color. With the largest demographic being of people with Asian background. The second largest demographic 
        being of people of Caucasian background. The data shows that the people of color population have 
        increased in 2020 by about 19%. However, the demographics are disproportionate, as certain groups such as 
        Asian, Caucasian, Hispanic are increasing in population. Whereas other groups such as African American, 
        Native American and Pacific Islander have decreased by the year 2020."),
      h3("Population Demographics in Beacon Hill"),
      h4("Populations in 2010 versus 2020"),
      sidebarLayout(
        sidebarPanel(
          selectInput(
            inputId = "beacon_demo",
            label = "Choose what demographics to see in this neighborhood",
            choices = c("All", "Total", "White", "Black", "Asian", "Native", "Pacific Islander", "Hispanic", "Other", "Two or More")
          )
        ),
        mainPanel(
          plotOutput(outputId = "beacon_bar")
        )
      ), 
      h4("Percent changes"), 
      h5("This shows the percentage increase of each of these demographics in Beacon Hill"),
      sidebarLayout(
        sidebarPanel(
          h5("Decide if you'd like to include outliers:"),
          checkboxInput(
            inputId = "incl_beacon_ol",
            label = "Include Outliers", 
            value = TRUE
          )
        ), 
        mainPanel(
          plotlyOutput(outputId = "beacon_perc_bar")
        )
      ),
      h5("This shows the percentage decrease of each of these demographics in Beacon Hill"),
      plotlyOutput(outputId = "beacon_perc_dec")
    ),
    tabPanel(
      "International District",
      h3("About the neighborhood:"),
      p("Similar to Beacon Hill, International districts largest demographic is of people with Asian 
        background in 2010. Along with the second largest demographic being of people of Caucasian 
        background. The population of people of color has increased by approximately 36%. While each 
        demographic varies in population, all but one group of people of color shows an increase in 
        population at International district by 2020. Pacific Islander being the outlier, its population
        decreased by approximately 39%."),
      h3("Population Demographics in International District"),
      sidebarLayout(
        sidebarPanel(
          selectInput(
            inputId = "international_demo",
            label = "Choose what demographics to see in this neighborhood",
            choices = c("All", "Total", "White", "Black", "Asian", "Native", "Pacific Islander", "Hispanic", "Other", "Two or More")
          )
        ),
        mainPanel(
          plotOutput(outputId = "international_bar")
        )
      ), 
      h4("Percent Changes"),
      h5("This shows the percentage increase of each of these demographics in the International District"),
      sidebarLayout(
        sidebarPanel(
          h5("Decide if you'd like to include outliers:"),
          checkboxInput(
            inputId = "incl_international_ol",
            label = "Include Outliers",
            value = TRUE
          )
        ),
        mainPanel(
          plotlyOutput(outputId = "international_perc_bar")
        )
      ), 
      h5("This shows the percentage decrease of each of these demographics in the International District"),
      plotlyOutput(outputId = "international_perc_dec")
    ),
    tabPanel(
      "Central District",
      h3("About the neighborhood:"),
      p("Central district, the neighborhood with the largest demographic being of people with Caucasian 
        background in 2010 yet it has highest population of African Americans out of all three neighborhoods 
        shown in 2010. Central district is known for being a predominantly African American neighborhood for 
        generations. However, the data shows that by 2020, the African American population has decreased by 
        approximately 12%. Along with Pacific Islander population decreasing by approximately 33%. The Asian 
        population skyrocketed in Central District by approximately 66% from 2010 to the year 2020."),
      h3("Population Demographics in Central District"),
      sidebarLayout(
        sidebarPanel(
          selectInput(
            inputId = "central_demo",
            label = "Choose what demographics to see in this neighborhood",
            choices = c("All", "Total", "White", "Black", "Asian", "Native", "Pacific Islander", "Hispanic", "Other", "Two or More")
          )
        ),
        mainPanel(
          plotOutput(outputId = "central_bar")
        )
      ), 
      h4("Percent Changes"),
      h5("This shows the percentage increase of each of these demographics in the Central District"),
      sidebarLayout(
        sidebarPanel(
          h5("Decide if you'd like to include outliers:"),
          checkboxInput(
            inputId = "incl_central_ol",
            label = "Include Outliers",
            value = TRUE
          )
        ),
        mainPanel(
          plotlyOutput(outputId = "central_perc_bar")
        )
      ), 
      h5("This shows the percentage decrease of each of these demographics in the Central District"),
      plotlyOutput(outputId = "central_perc_dec")
    )
  )
)

ui <- navbarPage(
  "Gentrification in Seattle Neighborhoods",
  tabPanel(
    "Story",
    story_view
  ), 
  tabPanel(
    "Analysis",
    analysis_view
  )
)

server <- function(input, output) {
  
  output$beacon_bar <- renderPlot({
    
    filt_df <- beacon_df
    
    if(input$beacon_demo != "All") {
      filt_df <- filter(filt_df, demographic == input$beacon_demo)
    }
    
    
    beacon_bar <- ggplot(data = filt_df, aes(x = demographic, y = population, fill = year)) +
      geom_col(position = position_dodge()) +
      labs(x = "Demographic", y = "Population", fill = "Year")
    
    plot(beacon_bar)
  })
  
  output$international_bar <- renderPlot({
    
    filt_df <- international_df
    
    if(input$international_demo != "All") {
      filt_df <- filter(filt_df, demographic == input$international_demo)
    }
    
    international_bar <- ggplot(data = filt_df, aes(x = demographic, y = population, fill = year)) +
      geom_col(position = position_dodge()) +
      labs(x = "Demographic", y = "Population", fill = "Year")
    
    plot(international_bar)
  })
  
  output$central_bar <- renderPlot({
    
    filt_df <- central_df
    
    if(input$central_demo != "All") {
      filt_df <- filter(filt_df, demographic == input$central_demo)
    }
    
    central_bar <- ggplot(data = filt_df, aes(x = demographic, y = population, fill = year)) +
      geom_col(position = position_dodge()) +
      labs(x = "Demographic", y = "Population", fill = "Year")
    
    plot(central_bar)
  })
  
  output$beacon_perc_bar <- renderPlotly({
    
    filt_df <- filter(beacon_change_df, perc_change > 0)
    
    if(input$incl_beacon_ol == FALSE) {
      filt_df <- filter(filt_df, perc_change < 150)
    }
    
    beacon_perc_bar <- ggplot(data = filt_df, aes(x = demographic, y = perc_change, fill = demographic)) + 
      geom_col() + 
      labs(x = "Demographic", y = "Percentage Change", fill = "Demographic")
    
    beacon_perc_bar <- ggplotly(beacon_perc_bar, tooltip = "y")
  })
  
  output$international_perc_bar <- renderPlotly({
    
    filt_df <- filter(international_change_df, perc_change > 0)
    
    if(input$incl_international_ol == FALSE) {
      filt_df <- filter(filt_df, perc_change < 150)
    }
    
    international_perc_bar <- ggplot(data = filt_df, aes(x = demographic, y = perc_change, fill = demographic)) +
      geom_col() + 
      labs(x = "Demographic", y = "Percentage Change", fill = "Demographic")
    
    international_perc_bar <- ggplotly(international_perc_bar, tooltip = "y")
  })
  
  output$central_perc_bar <- renderPlotly({
    
    filt_df <- filter(central_change_df, perc_change > 0)
    
    if(input$incl_central_ol == FALSE) {
      filt_df <- filter(filt_df, perc_change < 150)
    }
    
    central_perc_bar <- ggplot(data = filt_df, aes(x = demographic, y = perc_change, fill = demographic)) +
      geom_col() + 
      labs(x = "Demographic", y = "Percentage Change", fill = "Demographic")
    
    central_perc_bar <- ggplotly(central_perc_bar, tooltip = "y")
  })
  
  output$beacon_perc_dec <- renderPlotly({
    
    filt_df <- filter(beacon_change_df, perc_change < 0)
    
    beacon_perc_dec <- ggplot(data = filt_df, aes(x = demographic, y = perc_change, fill = demographic)) +
      geom_col() + 
      labs(x = "Demographic", y = "Percentage Change", fill = "Demographic")
    
    beacon_perc_dec <- ggplotly(beacon_perc_dec, tooltip = "y")
  })
  
  output$international_perc_dec <- renderPlotly({
    
    filt_df <- filter(international_change_df, perc_change < 0)
    
    international_perc_dec <- ggplot(data = filt_df, aes(x = demographic, y = perc_change, fill = demographic)) + 
      geom_col() + 
      labs(x = "Demographic", y = "Percentage Change", fill = "Demographic") 
    
    international_perc_dec <- ggplotly(international_perc_dec, tooltip = "y")
  })
  
  output$central_perc_dec <- renderPlotly({
    
    filt_df <- filter(central_change_df, perc_change < 0)
    
    central_perc_dec <- ggplot(data = filt_df, aes(x = demographic, y = perc_change, fill = demographic)) + 
      geom_col() + 
      labs(x = "Demographic", y = "Percentage Change", fill = "Demographic") 
    
    central_perc_dec <- ggplotly(central_perc_dec, tooltip = "y")
  })
  
}

shinyApp(ui, server)



# Shiny App for the project "How Terrorism Does (Not) Affect Political Attitudes"
# Date: 21 May 2021
# Code by Martin Lukac (m.b.lukac@gmail.com)

library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(tidyverse)


metadata <- read_csv("main_data.csv") %>%
    select(ID_R, country, design, terrortype, outcome, Fisher, Variance_F, SE_F)
reports <- read_csv("reports_data.csv") %>%
    select(ID_R, No., `Author(s)`, Year, Title, Journal)

ui <- fluidPage(
    
    
    titlePanel("How Terrorism Does (and Does Not) Affect Political Attitudes"),
    
    
    sidebarLayout(
        # Inputs panel
        sidebarPanel(
            pickerInput("region", 
                        label = h4("Region of Study"),
                        choices = list("United States" = "United States",
                                       "Israel" = "Israel",
                                       "Other: Western" = "Other: Western",
                                       "Other: Non-Western" = "Other: Non-Western"),
                        selected = c("United States", 
                                     "Israel", 
                                     "Other: Western",
                                     "Other: Non-Western"),
                        options = list(
                            `actions-box` = TRUE), 
                        multiple = TRUE
            ),
            pickerInput("design", label = h4("Study design"),
                        choices = list("Correlation" = "Correlation", 
                                       "(Quasi)Experiment" = "(Quasi)Experiment",
                                       "Longitudinal" = "Longitudinal"),
                        selected = c("Correlation", 
                                     "(Quasi)Experiment",
                                     "Longitudinal"),
                        options = list(
                            `actions-box` = TRUE), 
                        multiple = TRUE),
            pickerInput("type", label = h4("Terrorist type"),
                        choices = list("Islamist" = "Islamist", 
                                       "Extreme Right" = "Extreme Right", 
                                       "No Ideology" = "No Ideology", 
                                       "Other" = "Other"),
                        select = c("Islamist", 
                                   "Extreme Right", 
                                   "No Ideology", 
                                   "Other"),
                        options = list(
                            `actions-box` = TRUE), 
                        multiple = TRUE),
            pickerInput("outcome", label = h4("Output category"),
                        choices = list("Outgroup Hostility" = "Outgroup Hostility", 
                                       "Conservatism" = "Conservatism", 
                                       "Rally Effects" = "Rally Effects"),
                        select = c("Outgroup Hostility", "Conservatism", 
                                   "Rally Effects"),
                        options = list(
                            `actions-box` = TRUE), 
                        multiple = TRUE)
        ),
        
        # Outputs panel
        mainPanel(
            tabsetPanel(type = "tabs",
                        
                        tabPanel("About", icon = icon("info-circle"),
                                 br(),
                                 div(class = "about",
                                     p("This meta-analysis takes stock of the quantitative literature on ", 
                                       strong("public responses to terrorism"), ". 
                                       The dataset includes 320 studies conducted between 1985 and 2020 on more than 400,000 respondents from over 30 countries. This web application provides access to the data and options to ", 
                                       strong("explore heterogeneity"), 
                                       "in how people across the world react to different types of terrorism."),
                                     
                                     p("In the panel on the left you can specify your desired settings, which will result in a subsample of the full dataset. You can filter the data based on the original method used, type of terrorism (independent variable) and attitudes (dependent variable) studied, and geographical region. All effect sizes included in your pre-specified subsample and the overall effect size will be plotted in the ", icon("bar-chart-o"), strong("Plot"), " tab. This plot displays the average Fisher’s Z correlation coefficient for the relationship between terrorism and public opinion based on your pre-defined settings. The ", icon("list-alt"), strong("Data"), " tab prints all primary studies on which the results plot is based. The length of this list gives an indication of remaining research gaps in this field of study."),
                                     
                                     p("This application accompanies the following study: Godefroidt, A. How Terrorism Does (and Does Not) Affect Citizens’ Political Attitudes (revise and resubmitted for publication), 2021."),
                                     p("You can download the source code for the Shiny App here and the full Replication File accompanying the article here."),
                                     hr(),
                                     p(icon("copyright"), a("Amélie Godefroidt", href = "https://www.ameliegodefroidt.com",
                                                            target = "_blank", rel = "noopener noreferrer"),
                                     " & ", a("Martin Lukac", href = "https://mblukac.github.io",
                                              target = "_blank", rel = "noopener noreferrer"), " 2021")
                                 )
                        ),
                        tabPanel("Data", icon = icon("list-alt"),
                                 DT::dataTableOutput("metadata_dt")
                        ),
                        tabPanel("Plot", icon = icon("bar-chart-o"),
                                 plotOutput("metaplot"))
            )
        )
    )
)


server <- function(input, output) {
    
    # Run the filtering as a reactive expression
    currentData <- reactive({
        metadata %>%
            filter(country %in% input$region,
                   design %in% input$design,
                   terrortype %in% input$type,
                   outcome %in% input$outcome)
    })
    
    # Render a data.table
    output$metadata_dt <- DT::renderDataTable({
        reports %>% 
            filter(reports$ID_R %in% currentData()$ID_R) %>%
            select(-ID_R, -No.)
    })
    
    # Render a plot of effects
    
    output$metaplot <- renderPlot({
        
        if(nrow(currentData()) != 0) {
            currentData() %>%
                arrange(Fisher) %>%
                mutate(id = seq.int(nrow(currentData())),
                       Fisher_lower = Fisher - 1.96 * SE_F,
                       Fisher_upper = Fisher + 1.96 * SE_F) %>%
                ggplot(aes(x = id, y = Fisher)) +
                geom_hline(yintercept = 0,
                           color = "black") +
                geom_linerange(aes(x = id, ymin = Fisher_lower, ymax = Fisher_upper),
                               color = "grey40", alpha = 0.2) +
                geom_point(size = 0.7) +
                geom_hline(yintercept = 0.1195298,
                           color = "red") +
                coord_flip() +
                theme_minimal() +
                theme(
                    axis.title.y = element_blank(),
                    axis.ticks.y = element_blank(),
                    axis.text.y = element_blank(),
                    panel.grid.major.y = element_blank(),
                    panel.grid.minor.y = element_blank()
                ) + ylab("Fisher's Z") +
                annotate("text", x = 1, y = 0.1195298,
                         label = "0.1195", color = "red", hjust = -0.2)
        } else {
            ggplot(data = data.frame(x = 10, y = 10)) +
                annotate("text", x = 5, y = 5, 
                         label = "Choose data\n(panel on the left)") +
                theme(axis.line = element_blank(),
                      axis.ticks = element_blank(),
                      axis.text = element_blank()) +
                xlab("") + ylab("")
        }
        
    })
}


shinyApp(ui = ui, server = server)

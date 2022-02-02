# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# PROJECT:   How Terrorism Does (and Does Not) Affect Citizens' Attitudes: 
#            A Meta-Analysis. 
#             
# AUTHOR:   Project and paper by Amelie Godefroidt (amelie.godefroidt@ntnu.no)
#           R code by Martin Lukac (m.b.lukac@gmail.com)
#           
# DATE:     Shiny App created on 21 May 2021
#           Last successful replication on 10 September 2021
# 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ # 
# This R code contains the code necessary to build the Shiny App

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ # 

# Get necessary packages
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(tidyverse)

# Get necessary data
metadata <- read_csv("main_data.csv") %>%
    select(ID_R, country, design, terrortype, outcome, Fisher, Variance_F, SE_F)
reports <- read_csv("reports_data.csv", 
                    locale = locale(encoding = "WINDOWS-1252")) %>%
    select(ID_R, No., `Author(s)`, Year, Title, Journal)
estimates <- readRDS("all-estimates.rds")

# Build the app
ui <- fluidPage(
    
    
    titlePanel("How Terrorism Does (and Does Not) Affect Political Attitudes"),
    
    
    sidebarLayout(
        # Inputs panel
        sidebarPanel(
            pickerInput("region", 
                        label = h4("Country of study"),
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
                                       "Experiment" = "Experiment",
                                       "Other" = "Other"),
                        selected = c("Correlation", 
                                     "Experiment",
                                     "Other"),
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
            pickerInput("outcome", label = h4("Outcome category"),
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
                                     p("This is the interactive appendix for the following study on public responses to terrorism:",
                                       tags$ul(tags$li("Godefroidt, A. (2022).", a("How Terrorism Does (and Does Not) Affect Citizens' Political Attitudes: A Meta-Analysis.", href = "https://onlinelibrary.wiley.com/doi/full/10.1111/ajps.12692", target = "_blank", rel = "noopener noreferrer"), em("American Journal of Political Science."))),
                                       p(),
                                       "This review study is based on a dataset of 241 manuscripts, which together account for 326 unique studies conducted between 1985 and 2020 among more than 400,000 respondents from approximately 30 countries. This web application allows users to ", strong("explore heterogeneity"), "in how people across the world react to different types of terrorism."),
                                     
                                     p("In the panel on the left you can specify your desired settings, which will result in a subsample of the full dataset. You can filter the data based on the (1) country of study, (2) study design, (3) type of terrorism, and/or (4) outcome variables used in the original study. All effect sizes included in your pre-specified subsample and the overall effect size will be plotted in the ", icon("bar-chart-o"), strong("Plot"), " tab. The vertical red line in this plot displays the average Fisher’s Z correlation coefficient for the relationship between terrorism and public opinion based on your predefined settings and using a three-level meta-analytic model (see",a("here",target="_blank",href="https://turtle-gold-8ha4.squarespace.com/s/Godefroidt_2021_SI.pdf"),
                                       "for more information on the meta-analytic model).", icon("list-alt"), strong("Data"), " tab prints all primary studies on which the results plot is based. The length of this list gives an indication of remaining research gaps in this field of study."),
                                     p("You can download the source code for the Shiny App", a("here",target="_blank",href="https://github.com/mblukac/terrorism-public-attitudes-metaanalysis"), "and the full Replication Repository accompanying the study", a("here.",target="_blank",href="https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/K4L5YI"),"If you have any further questions or comments, please do not hesitate to", icon("envelope-open-text"), strong("contact"), "me at", a("amelie.godefroidt@ntnu.no.", href = "mailto: amelie.godefroidt@ntnu.no", target = "_blank", rel = "noopener noreferrer")),
                                     hr(),
                                     p(style="text-align: right", 
                                       icon("copyright"),  a("Amélie Godefroidt", href = "https://www.ameliegodefroidt.com", target = "_blank", rel = "noopener noreferrer"), " & ", a("Martin Lukac", href = "https://mblukac.github.io", target = "_blank", rel = "noopener noreferrer"), " 2021"))),
                        tabPanel("Data", icon = icon("list-alt"),
                                 helpText(p(em("Note:"), "All primary studies that meet your predefined criteria are listed below. Perceptive users might note that some key studies are missing from this list. This is due to the fact that some studies apply a different unit of analysis, do not contain enough information to calculate a common effect size and/or its variance, or focus on different dependent and/or independent variables. See Table B.1. in the", a("Supplementary Information", href="https://turtle-gold-8ha4.squarespace.com/s/Godefroidt-Terrorism_and_Attitudes-SIR1.pdf"), "for the full list of inclusion and exclusion rules applied during data collection."),
                                          style = "font-size:12px"),
                                 DT::dataTableOutput("metadata_dt")
                        ),
                        tabPanel("Plot", icon = icon("bar-chart-o"),
                                 helpText(p(em("Note:"),"The graph needs a few moments to load. In addition, the overall effect sizes reported in this graph might slightly differ from the ones reported in Tables C.1-3 in the", a("Supplementary Information (SI)", href="https://turtle-gold-8ha4.squarespace.com/s/Godefroidt-Terrorism_and_Attitudes-SIR1.pdf"), "because we subsetted the data to calculate all estimates for the app, but used moderation effects on the entire dataset (for each outcome measure) in the SI."), 
                                          style = "font-size:12px"),
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
    
    currentEst <- reactive({
        isHere <- map(1:length(estimates), 
                      function(x) estimates[[x]]$input %in% c(input$region, 
                                                              input$design, 
                                                              input$type, 
                                                              input$outcome))
        isHereFilter <- unlist(map(1:length(isHere), 
                                   function(x) all(isHere[[x]] == T)))
        isCorrectLength <- unlist(map(1:length(estimates),
                                      function(x) length(estimates[[x]]$input) == length(c(input$region, 
                                                                                           input$design, 
                                                                                           input$type, 
                                                                                           input$outcome))))
        as.numeric(paste(estimates[isHereFilter & isCorrectLength][[1]]$b))
    })
    
    # Render a data.table
    output$metadata_dt <- DT::renderDataTable({
        reports %>% 
            filter(reports$ID_R %in% currentData()$ID_R) %>%
            select(-ID_R, -No.) %>%
            arrange(`Author(s)`)
    })
    
    # Render a plot of overall effects
    output$metaplot <- renderPlot({
        
        if(nrow(currentData()) > 2) {
            currentData() %>%
                arrange(Fisher) %>%
                mutate(id = seq.int(nrow(currentData())),
                       Fisher_lower = Fisher - 1.96 * SE_F,
                       Fisher_upper = Fisher + 1.96 * SE_F) %>%
                ggplot(aes(x = id, y = Fisher)) +
                geom_hline(yintercept = 0,
                           color = "black") +
                geom_linerange(aes(x = id, 
                                   ymin = Fisher_lower, 
                                   ymax = Fisher_upper),
                               color = "grey80", alpha = 0.2) +
                geom_point(size = 0.7) +
                geom_hline(yintercept = currentEst(),
                           color = "#d13b3b") +
                coord_flip() +
                scale_y_continuous(limits=c(-1, 1)) +
                theme_bw() +
                theme(
                    axis.title.y = element_blank(),
                    axis.ticks.y = element_blank(),
                    axis.text.y = element_blank(),
                    axis.ticks.length.x = unit(0.3, "cm"),
                    axis.text.x = element_text(size = (14)),
                    axis.title.x = element_text(size = (17)),
                    panel.grid.major.y = element_blank(),
                    panel.grid.minor.y = element_blank()
                ) + ylab("Fisher's Z Correlation Coefficient") +
                annotate("text", x = 1, y = currentEst(),
                         label = paste(round(currentEst(), 3)), 
                         color = "#d13b3b", hjust = -0.15, size = 4.5)
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

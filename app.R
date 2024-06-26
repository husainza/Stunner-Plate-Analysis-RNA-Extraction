# Load necessary libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(DT)

# Define UI for the app
ui <- fluidPage(
    titlePanel("96-Well Plate Concentration Analysis"),
    sidebarLayout(
        sidebarPanel(
            fileInput("file1", "Choose CSV File",
                      accept = c(
                          "text/csv",
                          "text/comma-separated-values,text/plain",
                          ".csv")
            ),
            tags$hr(),
            checkboxInput("header", "Header", TRUE)
        ),
        mainPanel(
            tabsetPanel(
                tabPanel("Plate Plot", plotOutput("platePlot")),
                tabPanel("Summary Statistics", DTOutput("summaryTable")),
                tabPanel("Histogram", plotOutput("histPlot")),
                tabPanel("Density Plot", plotOutput("densityPlot")),
                tabPanel("Box Plot", plotOutput("boxPlot")),
                tabPanel("Grouped Analysis",
                         selectInput("groupVariable", "Select Group Variable", choices = c("Sample.Name")),
                         DTOutput("groupedSummaryTable"),
                         plotOutput("groupedHistPlot"),
                         plotOutput("groupedDensityPlot"),
                         plotOutput("groupedBoxPlot")
                )
            )
        )
    )
)

# Define server logic for the app
server <- function(input, output) {
    dataInput <- reactive({
        inFile <- input$file1
        
        if (is.null(inFile))
            return(NULL)
        
        # Read the uploaded file
        data <- read.csv(inFile$datapath, header = input$header)
        
        # Extract relevant columns and process data
        data <- data %>%
            select(Sample.Name = `Sample.Name`, Well.Position = `Well.Positon`, Concentration = `Concentration..ng.ul.`) %>% 
            mutate(Plate.Position = sub(".*:", "", Well.Position),
                   Row = substr(Plate.Position, 1, 1),
                   Column = as.numeric(substr(Plate.Position, 2, 3)))
        
        # Convert Row to factor with correct levels
        data$Row <- factor(data$Row, levels = rev(LETTERS[1:8]))
        
        return(data)
    })
    
    output$platePlot <- renderPlot({
        data <- dataInput()
        if (is.null(data))
            return(NULL)
        
        ggplot(data, aes(x = Column, y = Row, fill = Concentration)) +
            geom_point(shape = 21, size = 14, color = "black") +
            scale_fill_gradient(low = "white", high = "red") +
            labs(title = "96-Well Plate Concentration Plot",
                 x = "",
                 y = "",
                 fill = "Concentration (ng/ul)") +
            theme_minimal(base_size = 16) +
            theme(axis.text.x = element_text(hjust = 0),
                  axis.title.x = element_text(vjust = -1),
                  axis.title.y = element_text(vjust = 2),
                  axis.text.x.bottom = element_blank(),
                  axis.ticks.x.bottom = element_blank(),
                  axis.title.x.bottom = element_blank(),
                  plot.margin = margin(t = 40, r = 20, b = 20, l = 20, unit = "pt")) +
            scale_x_continuous(position = "top", breaks = seq(1, 12, 1)) + 
            geom_label(aes(label = Concentration), vjust = 0.5, hjust = 0.5)
    })
    
    output$summaryTable <- renderDT({
        data <- dataInput()
        if (is.null(data))
            return(NULL)
        
        summaryStats <- data %>%
            summarise(
                Mean = mean(Concentration, na.rm = TRUE),
                Median = median(Concentration, na.rm = TRUE),
                SD = sd(Concentration, na.rm = TRUE),
                CV = SD / Mean * 100,
                Min = min(Concentration, na.rm = TRUE),
                Q1 = quantile(Concentration, 0.25, na.rm = TRUE),
                Q3 = quantile(Concentration, 0.75, na.rm = TRUE),
                Max = max(Concentration, na.rm = TRUE)
            )
        summaryStats <- signif(summaryStats, 3)
        datatable(summaryStats)
    })
    
    output$histPlot <- renderPlot({
        data <- dataInput()
        if (is.null(data))
            return(NULL)
        
        ggplot(data, aes(x = Concentration)) +
            geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
            labs(title = "Histogram of Concentrations",
                 x = "Concentration (ng/ul)",
                 y = "Frequency") +
            theme_minimal(base_size = 16)
    })
    
    output$densityPlot <- renderPlot({
        data <- dataInput()
        if (is.null(data))
            return(NULL)
        
        ggplot(data, aes(x = Concentration)) +
            geom_density(fill = "green", alpha = 0.5) +
            labs(title = "Density Plot of Concentrations",
                 x = "Concentration (ng/ul)",
                 y = "Density") +
            theme_minimal(base_size = 16)
    })
    
    output$boxPlot <- renderPlot({
        data <- dataInput()
        if (is.null(data))
            return(NULL)
        
        ggplot(data, aes(y = Concentration)) +
            geom_boxplot(fill = "orange", color = "black", alpha = 0.7) +
            labs(title = "Box Plot of Concentrations",
                 y = "Concentration (ng/ul)") +
            theme_minimal(base_size = 16)
    })
    
    output$groupedSummaryTable <- renderDT({
        data <- dataInput()
        if (is.null(data))
            return(NULL)
        
        groupVar <- input$groupVariable
        summaryStats <- data %>%
            group_by_at(groupVar) %>%
            summarise(
                Mean = mean(Concentration, na.rm = TRUE),
                Median = median(Concentration, na.rm = TRUE),
                SD = sd(Concentration, na.rm = TRUE),
                CV = SD / Mean * 100,
                Min = min(Concentration, na.rm = TRUE),
                Q1 = quantile(Concentration, 0.25, na.rm = TRUE),
                Q3 = quantile(Concentration, 0.75, na.rm = TRUE),
                Max = max(Concentration, na.rm = TRUE)
            )
        summaryStats <- summaryStats %>%
            mutate_if(is.numeric, signif, digits = 3)
        datatable(summaryStats)
    })
    
    output$groupedHistPlot <- renderPlot({
        data <- dataInput()
        if (is.null(data))
            return(NULL)
        
        groupVar <- input$groupVariable
        ggplot(data, aes(x = Concentration, fill = .data[[groupVar]])) +
            geom_histogram(binwidth = 1, color = "black", alpha = 0.7, position = "dodge") +
            labs(title = paste("Histogram of Concentrations by", groupVar),
                 x = "Concentration (ng/ul)",
                 y = "Frequency") +
            theme_minimal(base_size = 16)
    })
    
    output$groupedDensityPlot <- renderPlot({
        data <- dataInput()
        if (is.null(data))
            return(NULL)
        
        groupVar <- input$groupVariable
        ggplot(data, aes(x = Concentration, color = .data[[groupVar]], fill = .data[[groupVar]])) +
            geom_density(alpha = 0.5) +
            labs(title = paste("Density Plot of Concentrations by", groupVar),
                 x = "Concentration (ng/ul)",
                 y = "Density") +
            theme_minimal(base_size = 16)
    })
    
    output$groupedBoxPlot <- renderPlot({
        data <- dataInput()
        if (is.null(data))
            return(NULL)
        
        groupVar <- input$groupVariable
        ggplot(data, aes(x = .data[[groupVar]], y = Concentration, fill = .data[[groupVar]])) +
            geom_boxplot(color = "black", alpha = 0.7) +
            labs(title = paste("Box Plot of Concentrations by", groupVar),
                 x = groupVar,
                 y = "Concentration (ng/ul)") +
            theme_minimal(base_size = 16)
    })
}

# Run the app
shinyApp(ui = ui, server = server)

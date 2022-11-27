library(shiny)
library(shinyWidgets)
library(dplyr)
library(readxl)
library(ggplot2)
library(plotly)

students<-read_excel("foreign_students.xlsx")

# Get selection lists
genders <- sort(unique(students$gender))
countries <- sort(unique(students$country))
university_types <- sort(unique(students$university_type))
universities <- sort(unique(students$university))
nationalities <- sort(unique(students$nationality))

ui <- fluidPage(
  titlePanel("Foreign Students by Nationality"),
  sidebarPanel(
    checkboxGroupInput(inputId = "i_gender",
                       label = "Select Gender(s):",
                       choices = genders,
                       selected = genders),
    pickerInput(inputId = "i_country", 
                label = "Select Country(s):", 
                choices = countries,
                options = list(`actions-box` = TRUE),
                selected = "İSTANBUL",
                multiple = TRUE),
    pickerInput(inputId = "i_uni_type", 
                label = "Select University Type(s):", 
                choices = university_types,
                options = list(`actions-box` = TRUE),
                selected = "DEVLET",
                multiple = TRUE),
    pickerInput(inputId = "i_university", 
                label = "Select University(s):", 
                choices = universities,
                options = list(`actions-box` = TRUE),
                selected = "BOĞAZİÇİ ÜNİVERSİTESİ",
                multiple = TRUE),
    pickerInput(inputId = "i_nationality", 
                label = "Select Nationality(s):", 
                choices = nationalities,
                options = list(`actions-box` = TRUE),
                selected = "SURİYE ARAP CUMHURİYETİ",
                multiple = TRUE)
  ),
  # MODIFY CODE BELOW: Create a tab layout for the dashboard
  mainPanel(
    
    tabsetPanel(
      
      tabPanel("Table", DT::dataTableOutput(outputId = "studentlist")),
      tabPanel("Plot", plotly::plotlyOutput(outputId = "genderdist"))
      
    )
    
  )
)

server <- function(input, output) {
  
  studentlist <- reactive({
    filter(students, (country %in% input$i_country) &
             (university_type %in% input$i_uni_type) &
             (university %in% input$i_university) &
             (nationality %in% input$i_nationality)) %>%
      select(university:count)
  })
  
  output$studentlist <- DT::renderDataTable({
    DT::datatable(data = studentlist(), options = list(pageLength = 10),
                  rownames = FALSE, class = 'display', escape = FALSE)
    
  })
  
  output$genderdist <- plotly::renderPlotly({
    studentlist() %>%
      group_by(gender) %>%
      summarise(count = sum(count)) %>%
      ggplot(aes(x = "", y = count, fill = gender)) +
      geom_bar(stat = "identity", width=1) +
      geom_text(aes(label = count),
                position = position_stack(vjust = 0.5)) +
      labs(title = "Gender Distribution") +
      xlab("") +
      ylab("")
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
case_studies_module_ui <- function() {
  tabPanel(
    "Industry Case Studies",
    fluidPage(
      titlePanel("Industry Case Studies"),
      p("Examples of companies applying AI in the food industry."),
      DT::DTOutput("caseStudiesTable")
    )
  )
}

case_studies_module_server <- function(input, output, session) {
  case_data <- read.csv("case_studies.csv", stringsAsFactors = FALSE)

  output$caseStudiesTable <- DT::renderDT({
    DT::datatable(
      case_data,
      rownames = FALSE,
      options = list(pageLength = 10, autoWidth = TRUE)
    )
  })
}

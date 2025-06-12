help_module_ui <- function() {
  tabPanel(
    "Help",
    fluidPage(
      div(
        style = "max-width:1000px; margin:auto; line-height:1.6; font-size:16px;",
        h2("Using the Dashboard"),
        h3("Navigation"),
        p("Each tab provides access to a different part of the application:"),
        tags$ul(
          tags$li(strong("Home:"), " introduction to the project and its aims."),
          tags$li(strong("Methodology:"), " details on how the bibliometric dataset was compiled."),
          tags$li(strong("Food AI Review Database:"), " searchable table of the 128 review articles."),
          tags$li(strong("Review Article Visualisations:"), " interactive plots exploring publication trends and topics."),
          tags$li(strong("Research Article Analysis:"), " charts generated from the articles cited by the reviews."),
          tags$li(strong("Custom LLM Tools:"), " links to language model tools trained on this corpus."),
          tags$li(strong("Image Gallery:"), " collection of AI diagrams from the literature.")
        ),
        h3("Data Requirements"),
        p("Ensure the following files are present in the repository root:"),
        tags$ul(
          tags$li(code("FoodAI_Feb2025.bib"), " – bibliographic records for the review articles."),
          tags$li(code("openalex_minimal.csv"), " – metadata for referenced research articles."),
          tags$li(code("titles_and_abstracts.csv"), " – titles and abstracts retrieved from OpenAlex.")
        ),
        h3("Associated Paper"),
        p("This dashboard accompanies our systematic review of AI in food science and engineering."),
        p(
          tags$a(
            href = "https://doi.org/10.0000/placeholder",
            "Read the paper online"
          )
        )
      )
    )
  )
}

help_module_server <- function(input, output, session) {
  # No server-side logic for the Help tab
}

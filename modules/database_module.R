library(DT)

database_module_ui <- function() {
  fluidPage(
    titlePanel("Food AI Review Database"),
    p("Browse our curated collection of Food AI review articles below."),
    fluidRow(
      column(
        width = 12,
        withSpinner(
          DTOutput("mainTableUI"),
          type = 6,
          color = "#2C3E50"
        )
      )
    )
  )
}

database_module_server <- function(input, output, session) {
  output$mainTableUI <- renderDT({
    data_display <- data_main %>%
      select(-ID, -DocID) %>%
      rename(
        "Title"              = TI,
        "Publication Year"   = PY,
        "Document Type"      = DT,
        "Source"             = SO,
        "Authors"            = AU,
        "Affiliations"       = affiliations,
        "Keywords"           = DE,
        "Subject Categories" = SC,
        "Abstract"           = AB,
        "DOI"                = DI
      ) %>%
      mutate_if(is.character, ~ str_replace_all(., "\\&", "&")) %>%
      mutate(Authors = Authors %>%
               str_replace_all(";", "; ") %>%
               str_to_title())

    datatable(
      data_display,
      escape   = FALSE,
      selection = 'none',
      options = list(
        pageLength = 10,
        autoWidth  = TRUE,
        scrollX    = TRUE,
        dom        = '<"top"if>rt<"bottom"ip><"clear">',
        columnDefs = list(list(width = '500px', targets = which(names(data_display) == "Abstract"))),
        rowCallback = JS(
          "function(row, data) {",
          "  $(row).css('cursor', 'pointer');",
          "  $(row).on('click', function() {",
          "    var doi = data[11];",
          "    if(doi) { window.open('https://doi.org/' + doi, '_blank'); }",
          "  });",
          "}"
        )
      )
    )
  })
}

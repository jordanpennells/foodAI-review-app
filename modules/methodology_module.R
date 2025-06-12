methodology_module_ui <- function() {
  fluidPage(
    div(style = "max-width:1000px; margin:auto; line-height:1.6; font-size:16px;",
        h2("Methodology"),
        h3("Background"),
        p("The increasing prominence of AI in food science is reflected in the growing number of research publications exploring its applications across the industry. As AI continues to advance, researchers and industry professionals rely on review articles to synthesise the vast body of knowledge and provide structured insights into how AI is transforming the industry from farm to fork. These reviews highlight key technological trends, emerging innovations, and the challenges associated with AI adoption in food systems. Given the increasing number of AI-related reviews in food science, bibliometric analysis serves as a valuable tool for mapping research developments, identifying dominant themes, and uncovering gaps in the field. Traditional bibliometric techniques, such as publication trend analysis, co-citation networks, and abstract-based topic modelling, offer a data-driven perspective on how food AI research has evolved over time. However, with the sheer volume of publications available, innovative approaches are needed to enhance accessibility and knowledge synthesis. To address this, we introduce an interactive online tool designed to complement this bibliometric analysis – the Food AI Dashboard App – which enables users to dynamically explore publication trends, keyword relationships, and thematic patterns in food AI literature. By combining bibliometric analysis with AI-driven knowledge synthesis, this section aims to provide a comprehensive overview of food AI research while enhancing the way users engage with the literature."),
        h3("Data Collection"),
        p("To conduct a comprehensive bibliometric analysis, we utilised the Web of Science (WoS) Core Collection database as of February 2025. Our search strategy targeted review articles related to AI and digital technology applications in food science and engineering."),
        p("Table 1: Categorisation of review articles related to AI and digital technology applications across key categories of the food system:"),
        tags$table(
          tags$thead(
            tags$tr(
              tags$th("Category"),
              tags$th("Number of Review Articles")
            )
          ),
          tags$tbody(
            tags$tr(tags$td("Food Safety"), tags$td("39")),
            tags$tr(tags$td("Process Monitoring, Control & Optimisation"), tags$td("37")),
            tags$tr(tags$td("General AI"), tags$td("35")),
            tags$tr(tags$td("Product Quality"), tags$td("28")),
            tags$tr(tags$td("Traceability"), tags$td("23")),
            tags$tr(tags$td("Sensory Evaluation"), tags$td("14")),
            tags$tr(tags$td("Supply Chain Optimisation"), tags$td("10")),
            tags$tr(tags$td("Multi-OMICS"), tags$td("7")),
            tags$tr(tags$td("Personalised Nutrition"), tags$td("5")),
            tags$tr(tags$td("Ingredient Quality"), tags$td("4")),
            tags$tr(tags$td("Synthetic Biology"), tags$td("4")),
            tags$tr(tags$td("Product Development"), tags$td("4")),
            tags$tr(tags$td("Intelligent Packaging"), tags$td("3")),
            tags$tr(tags$td("Augmented Reality"), tags$td("2")),
            tags$tr(tags$td("Food Security"), tags$td("1"))
          )
        ),
        br(),
        p(em("For further details, please refer to the associated review article [Insert DOI here]."))
    )
  )
}

methodology_module_server <- function(input, output, session) {
  # No server-side logic for the Methodology tab
}

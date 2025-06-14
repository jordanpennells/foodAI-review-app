visualizations_module_ui <- function() {
  # Safely compute year range values in case `data_main` is not yet available
  min_year <- if (exists("data_main")) min(as.numeric(data_main$PY), na.rm = TRUE) else NA
  max_year <- if (exists("data_main")) max(as.numeric(data_main$PY), na.rm = TRUE) else NA
  tabPanel("Review Article Visualisations",
           fluidPage(
             titlePanel("Review Article Visualisations"),
             tabsetPanel(
               
               # 1) Overview Tab => Data Summary
               tabPanel("Summary",
                        sidebarLayout(
                          sidebarPanel(
                            numericInput("topK", "Top n Results for Summary:",
                                         value = 8, min = 1, max = 50)
                          ),
                          mainPanel(
                            h4("Data Summary"),
                            withSpinner(
                              verbatimTextOutput("biblioSummaryNoAvg"),
                              type  = 6,
                              color = "#2C3E50"
                            )
                          )
                        )
               ),
               
               # 2) Year Distribution
               tabPanel("Publication Years",
                        sidebarLayout(
                          sidebarPanel(
                            sliderInput("yearRange", "Select Year Range:",
                                        min   = min_year,
                                        max   = max_year,
                                        value = c(2004, max_year),
                                        step  = 1, sep = ""),
                            wellPanel(
                              p("This bar chart shows the number of Food AI review articles published per year in the selected range.")
                            ),
                            actionButton("resetYearFilters", "Reset Filters")
                          ),
                          mainPanel(
                            withSpinner(
                              plotlyOutput("yearDistPlot", height = "600px"),
                              type  = 6,      # pick a spinner style 1–8
                              color = "#2C3E50"
                            ),
                            downloadButton("downloadYearDist", "Download Plot")
                          )
                        )
               ),
               
               # 3) Most Productive Authors
               tabPanel("Most Productive Authors",
                        sidebarLayout(
                          sidebarPanel(
                            sliderInput("topAuthors", "Select Top N Authors:",
                                        min = 5, max = 50, value = 10, step = 1)
                          ),
                          mainPanel(
                            withSpinner(
                              plotlyOutput("mpAuthorsPlot", height = "800px"),
                              type  = 6,      # pick a spinner style 1–8
                              color = "#2C3E50"
                            )
                          )
                        )
               ),
               
               # 8) Author Production Over Time
               tabPanel("Author Production Over Time",
                        sidebarLayout(
                          sidebarPanel(
                            sliderInput("authorTopK", "Top Authors (k):",
                                        min = 1, max = 50, value = 12),
                            sliderInput("authorYearRange", "Select Year Range:",
                                        min   = min_year,
                                        max   = max_year,
                                        value = c(min_year, max_year),
                                        step  = 1)
                          ),
                          mainPanel(
                            withSpinner(
                              plotlyOutput("authorProdOverTimePlot", height = "1000px"),
                              type  = 6,      # pick a spinner style 1–8
                              color = "#2C3E50"
                            )
                          )
                        ),
                        helpText("Adjust the top authors (k) and year range for the author production over time visualization.")
               ),
               
               # 4) Most Productive Countries
               tabPanel("Most Productive Countries",
                        sidebarLayout(
                          sidebarPanel(
                            sliderInput("topCountries", "Select Top n Countries:",
                                        min = 5, max = 50, value = 10, step = 1)
                          ),
                          mainPanel(
                            withSpinner(
                              plotlyOutput("mpCountriesPlot", height = "500px"),
                              type  = 6,      # pick a spinner style 1–8
                              color = "#2C3E50"
                            ),
                            tags$hr(),
                            h4("Geographical Distribution"),
                            plotlyOutput("mpCountriesMap", height = "800px")
                          )
                        )
               ),
               
               # 4) Country Production Over Time
               tabPanel("Country Production Over Time",
                        sidebarLayout(
                          sidebarPanel(
                            sliderInput("countryTopK", "Top Countries (k):",
                                        min = 1, max = 50, value = 10, step = 1),
                            sliderInput("countryYearRange", "Select Year Range:",
                                        min   = min_year,
                                        max   = max_year,
                                        value = c(min_year, max_year),
                                        step  = 1)
                          ),
                          mainPanel(
                            withSpinner(
                              plotlyOutput("countryProdOverTimePlot", height = "1000px"),
                              type  = 6, color = "#2C3E50"
                            )
                          )
                        ),
                        helpText("Adjust the top countries (k) and year range for the country production over time visualization.")
               ),
               
               
               # 9) Collaboration Analysis (Institutions)
               tabPanel("Collaboration Analysis",
                        fluidPage(
                          h4("Collaboration Analysis - Institutions"),
                          sliderInput("inst_n", "Show top N Institutions in network:", min = 5, max = 50, value = 20),
                          plotOutput("collabInstPlot", height = "800px")
                        )
               ),
               
               
               
               
               # 5) Most Relevant Sources
               tabPanel("Top Journals",
                        sidebarLayout(
                          sidebarPanel(
                            sliderInput("topSources", "Select Top N Sources:",
                                        min = 5, max = 50, value = 10, step = 1)
                          ),
                          mainPanel(
                            withSpinner(
                              plotlyOutput("mpSourcesPlot", height = "1000px"),
                              type  = 6,      # pick a spinner style 1–8
                              color = "#2C3E50"
                            )
                          )
                        )
               ),
               
               # 6) Top Keywords
               tabPanel("Top Keywords",
                        fluidPage(
                          sliderInput("topKeywords", "Number of Keywords:", min = 5, max = 50, value = 10, step = 1),
                          wellPanel(
                            p("This bar chart displays the most frequent author keywords in the review articles.")
                          ),
                          withSpinner(
                            plotlyOutput("keywordsPlot", height = "600px"),
                            type  = 6,      # pick a spinner style 1–8
                            color = "#2C3E50"
                          ),
                          tags$hr()
                        )
               ),
               
               # Tab for Abstract Terms
               tabPanel("Top Abstract Terms",
                        fluidPage(
                          sliderInput("topTerms", "Number of Abstract Terms:", min = 5, max = 50, value = 10, step = 1),
                          withSpinner(
                            plotlyOutput("abstractTermsPlot", height = "600px"),
                            type  = 6,      # pick a spinner style 1–8
                            color = "#2C3E50"
                          ),
                          tags$hr(),
                          h4("Articles Containing the Selected Term"),
                          DT::dataTableOutput("abstractArticles")
                        )
               ),
               
               # 7) Keyword Co-occurrence Network
               tabPanel("Keyword Co-occurrence Network",
                        fluidPage(
                          h4("Keyword Co-occurrence Network"),
                          fluidRow(
                            column(3,
                                   sliderInput("n_keywords", "Top n Keywords:", min = 5, max = 50, value = 50, step = 5)
                            ),
                            column(3,
                                   checkboxInput("normalize", "Normalize Network", value = FALSE)
                            ),
                            column(3,
                                   selectInput("layout_type", "Layout Type:",
                                               choices = c("fruchterman", "kamada.kawai", "circle"),
                                               selected = "fruchterman")
                            ),
                            column(3,
                                   sliderInput("node_size", "Node Size:", min = 1, max = 10, value = 5, step = 1),
                                   sliderInput("label_size", "Label Size:", min = 0.1, max = 2, value = 0.7, step = 0.1)
                            )
                          ),
                          fluidRow(
                            column(12,
                                   helpText("Top n Keywords: Number of top keywords to include."),
                                   helpText("Normalize Network: Toggle normalization of edge weights."),
                                   helpText("Layout Type: Select the algorithm for network layout."),
                                   helpText("Node Size: Adjust the size of nodes."),
                                   helpText("Label Size: Adjust the size of the labels on nodes.")
                            )
                          ),
                          fluidRow(
                            column(12, actionButton("resetKeywordFilters", "Reset Filters"))
                          ),
                          plotOutput("keywordCooccurPlot", height = "1000px")
                        )
               ),
               
               # 10) Thematic Map with interactive controls
               tabPanel("Thematic Map",
                        fluidPage(
                          h4("Thematic Map"),
                          fluidRow(
                            column(4,
                                   selectInput("tm_field", "Field for Terms:",
                                               choices = c("DE", "ID"), selected = "DE"),
                                   sliderInput("tm_n", "Number of Terms (n):", min = 50, max = 300, value = 150, step = 10),
                                   sliderInput("tm_minfreq", "Minimum Frequency:", min = 1, max = 20, value = 4, step = 1),
                                   checkboxInput("tm_stemming", "Apply Stemming", value = FALSE)
                            ),
                            column(4,
                                   sliderInput("tm_size", "Size Parameter:", min = 0.1, max = 1, value = 0.3, step = 0.1),
                                   sliderInput("tm_n_labels", "Number of Labels:", min = 1, max = 10, value = 4, step = 1),
                                   checkboxInput("tm_repel", "Repel Labels", value = TRUE)
                            ),
                            column(4,
                                   helpText("Field: The column (e.g., DE for author keywords or ID for keywords plus)."),
                                   helpText("n: Number of terms to include in the map."),
                                   helpText("minfreq: Minimum frequency for a term to be included."),
                                   helpText("Stemming: Apply stemming to merge similar terms."),
                                   helpText("Size: Adjusts the node size in the map."),
                                   helpText("n.labels: Number of terms to display as labels."),
                                   helpText("Repel: Toggle label repulsion for clarity.")
                            )
                          ),
                            plotOutput("thematicMapPlot", height = "1000px")
                        )
               ),
               
               # Tab: Bipartite Network
               tabPanel("Bipartite Network",
                        fluidPage(
                          sliderInput("edgeFilter", "Minimum Edge Count:",
                                      min = 1,
                                      max = max(adj_df$count),
                                      value = 1, step = 1),
                          helpText("Edges with count < this value will be filtered out."),
                          plotlyOutput("bipartitePlot", height = "1000px"),
                          br(),
                          fluidRow(
                            column(6,
                                   checkboxGroupInput("aiSelection", "AI Methods (Red):",
                                                      choices = names(ai_dictionary),
                                                      selected = names(ai_dictionary)),
                                   fluidRow(
                                     column(6, actionButton("aiSelectAll", "Select All")),
                                     column(6, actionButton("aiDeselectAll", "Deselect All"))
                                   )
                            ),
                            column(6,
                                   checkboxGroupInput("foodSelection", "Food Subdomains (Blue):",
                                                      choices = names(food_dictionary),
                                                      selected = names(food_dictionary)),
                                   fluidRow(
                                     column(6, actionButton("foodSelectAll", "Select All")),
                                     column(6, actionButton("foodDeselectAll", "Deselect All"))
                                   )
                            )
                          )
                        )
               ),
               
               # Tab: Similar Documents
               tabPanel("Similar Documents",
                        fluidPage(
                          titlePanel("Find Similar Documents"),
                          fluidRow(
                            column(12, h4("Topic Clusters of Review Articles"))
                          ),
                          fluidRow(
                            lapply(1:k, function(i) {
                              column(
                                2,
                                withSpinner(
                                  plotOutput(paste0("wordcloud_topic_", i), height = "200px"),
                                  type  = 6,
                                  color = "#2C3E50"
                                )
                              )
                            })
                          ),
                          tags$hr(),
                          fluidRow(
                            column(4,
                                   selectInput("selected_topic", "Select Topic:",
                                               choices = paste("Topic", 1:k),
                                               selected = "Topic 1")
                            ),
                            column(8,
                                   selectInput("selected_article", "Select Review Article:",
                                               choices = NULL,
                                               width = '600px')
                            )
                          ),
                          tags$hr(),
                          h4("Selected Article"),
                          withSpinner(
                            uiOutput("selected_article_details"),
                            type  = 6,
                            color = "#2C3E50"
                          ),
                          tags$hr(),
                          h4("Most Similar Documents"),
                          withSpinner(
                            DT::dataTableOutput("similar_documents"),
                            type  = 6,
                            color = "#2C3E50"
                          )

                          )            # close inner fluidPage
                        )            # close inner tabPanel
              )          # close tabsetPanel
            )            # close fluidPage
  )                      # close tabPanel
}

visualizations_module_server <- function(input, output, session) {
    #### (B) Data Summary in Overview (unchanged) ####
    output$biblioSummaryNoAvg <- renderPrint({
      raw_sum <- capture.output(
        summary(biblio_results, k = input$topK, pause = FALSE, width = 120)
      )
      filtered_sum <- raw_sum[!grepl("Average citations per doc", raw_sum)]
      filtered_sum <- filtered_sum[!grepl("Average citations per year per doc", filtered_sum)]
      cat(filtered_sum, sep = "\n")
    })
    
    #### (C) Year Distribution (interactive) ####
    yearDistPlotObj <- reactive({
      year_filtered <- data_main %>%
        filter(!is.na(PY)) %>%
        filter(as.numeric(PY) >= input$yearRange[1],
               as.numeric(PY) <= input$yearRange[2])

      if (nrow(year_filtered) == 0) {
        p <- ggplot() +
          geom_blank() +
          labs(title = "No publications in the selected range") +
          theme_minimal()
        return(p)
      }

      year_counts <- year_filtered %>%
        count(PY) %>%
        rename(n = n)

      p <- ggplot(year_counts, aes(x = factor(PY), y = n)) +
        geom_col(fill = "cornflowerblue") +
        labs(title = "Publication Year Distribution", x = "Year", y = "Count") +
        theme_minimal()
      p
    })

    output$yearDistPlot <- renderPlotly({
      ggplotly(yearDistPlotObj()) %>% layout(hovermode = "x")
    })

    output$downloadYearDist <- downloadHandler(
      filename = function() {
        paste0("year_distribution.png")
      },
      content = function(file) {
        ggsave(file, plot = yearDistPlotObj(), width = 8, height = 6)
      }
    )

    observeEvent(input$resetYearFilters, {
      updateSliderInput(session, "yearRange",
                        value = c(2004, max(as.numeric(data_main$PY), na.rm = TRUE)))
    })
    
    #### (D) Keywords (interactive) ####
    clickedKeyword <- reactiveVal(NULL)
    clickedTerm    <- reactiveVal(NULL)
    
    output$keywordsPlot <- renderPlotly({
      req(input$topKeywords)
      sumres <- summary(biblio_results, k = 200, pause = FALSE)
      df <- as.data.frame(sumres$MostRelKeywords)[,1:2]
      colnames(df) <- c("term","Count")
      df$Count <- as.numeric(df$Count)
      df <- df %>% filter(!is.na(Count), term != "") %>% unify_terms("term","Count")
      df <- head(df, input$topKeywords)
      
      p <- ggplot(df, aes(x = reorder(term, Count), y = Count,
                          text = paste0(term, ": ", Count),
                          customdata = term)) +
        geom_col(fill = "steelblue") +
        coord_flip() +
        labs(title = "Top Keywords", x = NULL, y = "Occurrences") +
        theme_minimal(base_size = 14)
      
      ggplotly(p, tooltip = "text", source = "keywordsPlot") %>%
        layout(clickmode = "event+select")
    })
    
    observeEvent(event_data("plotly_click", source = "keywordsPlot"), {
      ed <- event_data("plotly_click", source = "keywordsPlot")
      if (!is.null(ed) && length(ed$points)) {
        clickedKeyword(ed$points[[1]]$customdata)
      }
    })
    
    output$keywordsArticles <- renderDT({
      req(clickedKeyword())
      kw <- tolower(clickedKeyword())
      split_kw <- function(x) tolower(str_trim(unlist(strsplit(x,"[;,]"))))
      matched <- data_main %>%
        filter(sapply(DE, function(d) kw %in% split_kw(d))) %>%
        transmute(Title=TI, Year=PY, Journal=SO, Abstract=substr(AB,1,300))
      datatable(matched, options=list(pageLength=5), rownames=FALSE)
    })
    
    output$abstractTermsPlot <- renderPlotly({
      req(input$topTerms)
      df <- tidy_dtm %>% count(term, sort=TRUE) %>% rename(Count=n)
      df <- df %>% filter(!is.na(Count), term!="") %>% unify_terms("term","Count")
      df <- head(df, input$topTerms)
      
      p <- ggplot(df, aes(x = reorder(term, Count), y = Count,
                          text = paste0(term, ": ", Count),
                          customdata = term)) +
        geom_col(fill = "steelblue") +
        coord_flip() +
        labs(title = "Top Abstract Terms", x = NULL, y = "Occurrences") +
        theme_minimal(base_size = 14)
      
      ggplotly(p, tooltip="text", source="abstractPlot") %>%
        layout(clickmode = "event+select")
    })
    
    observeEvent(event_data("plotly_click", source="abstractPlot"), {
      ed <- event_data("plotly_click", source="abstractPlot")
      if (!is.null(ed)) clickedTerm(ed$points[[1]]$customdata)
    })
    
    output$abstractArticles <- renderDT({
      req(clickedTerm())
      term_chosen <- clickedTerm()
      matched <- data_main %>%
        filter(str_detect(tolower(AB), fixed(term_chosen))) %>%
        transmute(Title=TI, Year=PY, Journal=SO, Abstract=substr(AB,1,300))
      datatable(matched, options=list(pageLength=5), rownames=FALSE)
    })
    
    #### Interactive Most Productive Authors ####
    output$mpAuthorsPlot <- renderPlotly({
      sumres <- summary(biblio_results, k = input$topAuthors, pause = FALSE)
      df     <- as.data.frame(sumres$MostProdAuthors)[,1:2]
      colnames(df) <- c("Author","Count")
      df$Count <- as.numeric(df$Count)
      df_top   <- head(df, input$topAuthors)
      
      p <- ggplot(df_top, aes(x = reorder(Author, -Count), y = Count,
                              text = paste0(Author, ": ", Count))) +
        geom_col(fill = "steelblue") +
        labs(title = paste("Most Productive Authors (Top", input$topAuthors, ")"),
             x = "Author", y = "Articles") +
        theme_minimal(base_size = 14) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      ggplotly(p, tooltip="text") %>% layout(margin=list(b=150))
    })
    
    #### Interactive Most Productive Countries ####
    output$mpCountriesPlot <- renderPlotly({
      sumres <- summary(biblio_results, k = input$topCountries, pause = FALSE)
      df     <- as.data.frame(sumres$MostProdCountries)[,1:2]
      colnames(df) <- c("Country","Count")
      df$Count <- as.numeric(df$Count)
      df_top   <- head(df, input$topCountries)
      
      p <- ggplot(df_top, aes(x = reorder(Country, -Count), y = Count,
                              text = paste0(Country, ": ", Count))) +
        geom_col(fill = "steelblue") +
        labs(title = paste("Most Productive Countries (Top", input$topCountries, ")"),
             x = "Country", y = "Articles") +
        theme_minimal(base_size = 14) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      ggplotly(p, tooltip="text") %>% layout(margin=list(b=150))
    })
    
    #### Geographical Distribution (already interactive) ####
    # output$mpCountriesMap stays as renderPlotly()
    
    #### Interactive Keyword Co-occurrence Network ####
    output$keywordCooccurPlot <- renderPlot({
      net_mat <- biblioNetwork(data_main_biblio,
                               analysis = "co-occurrences",
                               network = "keywords",
                               sep = ";")
      topN <- input$n_keywords
      
      # Intercept the layout type if needed
      layout_type <- input$layout_type
      if(layout_type == "kamada.kawai"){
        layout_type <- "kamada"
      }
      
      networkPlot(net_mat,
                  normalize = if(input$normalize) TRUE else NULL,
                  n = topN,
                  Title = paste("Top", topN, "Keyword Co-occurrences"),
                  type = layout_type,
                  size = input$node_size,
                  labelsize = input$label_size)
    })

    observeEvent(input$resetKeywordFilters, {
      updateSliderInput(session, "n_keywords", value = 50)
      updateCheckboxInput(session, "normalize", value = FALSE)
      updateSelectInput(session, "layout_type", selected = "fruchterman")
      updateSliderInput(session, "node_size", value = 5)
      updateSliderInput(session, "label_size", value = 0.7)
    })
    
    
    
    #### Interactive Collaboration Analysis (Institutions) ####
    output$collabInstPlot <- renderPlot({
      inst_mat <- biblioNetwork(data_main_biblio,
                                analysis = "collaboration",
                                network = "universities",
                                sep = ";")
      networkPlot(inst_mat,
                  n = input$inst_n,
                  Title = "Collaboration Network (Institutions)",
                  type = "fruchterman",
                  size = 5,
                  labelsize = 0.7)
    })
    
    
    
    #### Interactive Top Journals ####
    output$mpSourcesPlot <- renderPlotly({
      sumres <- summary(biblio_results, k = input$topSources, pause = FALSE)
      df     <- as.data.frame(sumres$MostRelSources)[,1:2]
      colnames(df) <- c("Source","Count")
      df$Count <- as.numeric(df$Count)
      df_top   <- head(df, input$topSources)
      
      p <- ggplot(df_top, aes(x = reorder(Source, -Count), y = Count,
                              text = paste0(Source, ": ", Count))) +
        geom_col(fill = "steelblue") +
        labs(title = paste("Most Relevant Sources (Top", input$topSources, ")"),
             x = "Source", y = "Articles") +
        theme_minimal(base_size = 14) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
      
      ggplotly(p, tooltip="text") %>% layout(margin=list(b=150))
    })
    
    #### Interactive Author Production Over Time ####
    output$authorProdOverTimePlot <- renderPlotly({
      resAOT <- authorProdOverTime(data_main_biblio, k = input$authorTopK, graph = FALSE)
      df     <- resAOT$dfAU %>%
        filter(year >= input$authorYearRange[1],
               year <= input$authorYearRange[2])
      df_author_range <- df %>% 
        group_by(Author) %>% 
        summarize(min_year = min(year),
                  max_year = max(year),
                  total_articles = sum(freq, na.rm=TRUE), .groups="drop")
      df <- df %>%
        left_join(df_author_range, by="Author") %>%
        mutate(Author = factor(Author,
                               levels = rev(df_author_range$Author[order(-df_author_range$total_articles)])
        ))
      
      p <- ggplot(df, aes(x=year, y=Author,
                          text = paste0(Author, ": ", freq))) +
        geom_segment(aes(x=min_year, xend=max_year, yend=Author),
                     color="red", size=0.5) +
        geom_point(aes(size=freq, color=TCpY)) +
        scale_size_continuous(range=c(3,12), name="Articles") +
        scale_color_gradient(name="TC per Year",
                             low="lightblue", high="darkblue") +
        labs(title="Authors' Production Over Time",
             x="Year", y="Author") +
        theme_minimal(base_size = 14)
      
      ggplotly(p, tooltip="text") %>% layout(legend=list(x=0.8,y=0.1))
    })
    
    
    # ==== Country Production Over Time ====
    country_year_df <- reactive({
      library(dplyr); library(stringr)
      
      df <- data_main_biblio %>% 
        mutate(Year = as.integer(PY)) %>%
        filter(!is.na(C1), !is.na(Year))
      
      # We’ll treat “current year” as the max year in the data:
      currentYear <- max(df$Year, na.rm = TRUE)
      
      # For each paper, split C1 on “;”, take the last comma-piece as country,
      # compute that paper’s citations‐per‐year (TC / age),
      # and then stack into a big long table:
      entries <- lapply(seq_len(nrow(df)), function(i) {
        row <- df[i, ]
        # split into individual addresses
        addrs <- unlist(strsplit(row$C1, ";"))
        # extract country = last comma segment of each address
        countries <- unique(trimws(sapply(addrs, function(a) {
          parts <- strsplit(a, ",")[[1]]
          tail(trimws(parts), 1)
        })))
        # compute this paper’s citations‐per‐year
        age <- currentYear - row$Year + 1
        tc_per_year <- ifelse(is.na(row$TC), 0, row$TC / age)
        data.frame(Year       = row$Year,
                   Country    = countries,
                   TCpY_doc   = tc_per_year,
                   stringsAsFactors = FALSE)
      })
      
      bind_rows(entries) %>%
        filter(!is.na(Country)) %>%
        group_by(Country, Year) %>%
        summarise(
          freq = n(),                  # # papers that year
          TCpY = sum(TCpY_doc),        # sum of per‐paper citations/year
          .groups = "drop"
        )
    })
    
    output$countryProdOverTimePlot <- renderPlotly({
      library(dplyr); library(ggplot2); library(plotly)
      
      df_co <- country_year_df()
      
      # pick the top-K countries by total publications
      top_countries <- df_co %>%
        group_by(Country) %>%
        summarise(total = sum(freq), .groups="drop") %>%
        top_n(input$countryTopK, wt = total) %>%
        pull(Country)
      
      # restrict to those + the selected year range
      df <- df_co %>%
        filter(Country %in% top_countries,
               Year    >= input$countryYearRange[1],
               Year    <= input$countryYearRange[2])
      
      # get each country’s min/max span & overall total
      df_range <- df %>%
        group_by(Country) %>%
        summarise(
          min_year       = min(Year),
          max_year       = max(Year),
          total_articles = sum(freq),
          .groups = "drop"
        )
      
      # order factor by total_articles
      df <- df %>%
        left_join(df_range, by = "Country") %>%
        mutate(Country = factor(Country,
                                levels = rev(df_range$Country[order(-df_range$total_articles)])
        ))
      
      # build the ggplot
      p <- ggplot(df, aes(x = Year, y = Country,
                          text = paste0(Country, ": ", freq))) +
        geom_segment(aes(x = min_year, xend = max_year, yend = Country),
                     color = "red", size = 0.5) +
        geom_point(aes(size = freq, color = TCpY)) +
        scale_size_continuous(range = c(3,12), name = "Articles") +
        scale_color_gradient(name = "Citations / Year",
                             low  = "lightblue",
                             high = "darkblue") +
        labs(title = "Countries’ Production Over Time",
             x = "Year", y = "Country") +
        theme_minimal(base_size = 14)
      
      ggplotly(p, tooltip = "text") %>%
        layout(legend = list(x = 0.8, y = 0.1))
    })
    
    
    
    
    
    
    
    #### Interactive Thematic Map ####
    output$thematicMapPlot <- renderPlot({
      tm <- thematicMap(data_main_biblio,
                        field   = input$tm_field,
                        n       = input$tm_n,
                        minfreq = input$tm_minfreq,
                        stemming= input$tm_stemming,
                        size    = input$tm_size,
                        n.labels= input$tm_n_labels,
                        repel   = input$tm_repel)
      if(!is.null(tm$map)) {
        print(tm$map)
      } else {
        plot.new()
        text(0.5, 0.5, "No Thematic Map (check parameters or data).")
      }
    })

  
  
  # Bipartite Network
  wrap_label <- function(label, width = 12) {
    paste(strwrap(label, width = width), collapse = "\n")
  }
  
  clickedNode <- reactiveVal(NULL)
  
  observeEvent(input$aiSelectAll, {
    updateCheckboxGroupInput(session, "aiSelection", selected = names(ai_dictionary))
  })
  observeEvent(input$aiDeselectAll, {
    updateCheckboxGroupInput(session, "aiSelection", selected = character(0))
  })
  observeEvent(input$foodSelectAll, {
    updateCheckboxGroupInput(session, "foodSelection", selected = names(food_dictionary))
  })
  observeEvent(input$foodDeselectAll, {
    updateCheckboxGroupInput(session, "foodSelection", selected = character(0))
  })
  
  output$bipartitePlot <- renderPlotly({
    adj_filtered <- adj_df %>%
      filter(count >= input$edgeFilter) %>%
      filter(tolower(AI_Method) %in% tolower(input$aiSelection),
             tolower(Food_Subdomain) %in% tolower(input$foodSelection))
    
    if(nrow(adj_filtered) == 0) {
      no_data_plot <- ggplot() +
        geom_text(aes(0, 0, label = "No edges matching your selection.")) +
        theme_void()
      return(ggplotly(no_data_plot))
    }
    
    ai_nodes <- data.frame(id = input$aiSelection, type = TRUE, stringsAsFactors = FALSE)
    food_nodes <- data.frame(id = input$foodSelection, type = FALSE, stringsAsFactors = FALSE)
    nodes <- rbind(ai_nodes, food_nodes)
    
    if(nrow(ai_nodes) > 0) {
      ai_nodes$x <- seq(0.1, 0.9, length.out = nrow(ai_nodes))
      ai_nodes$y <- rep(1, nrow(ai_nodes))
    }
    if(nrow(food_nodes) > 0) {
      food_nodes$x <- seq(0.1, 0.9, length.out = nrow(food_nodes))
      food_nodes$y <- rep(0, nrow(food_nodes))
    }
    nodes <- rbind(ai_nodes, food_nodes)
    
    edges <- adj_filtered %>%
      filter(tolower(AI_Method) %in% tolower(nodes$id),
             tolower(Food_Subdomain) %in% tolower(nodes$id)) %>%
      rename(from = AI_Method, to = Food_Subdomain)
    
    if(nrow(edges) > 0) {
      edges <- merge(edges, nodes[, c("id", "x", "y")], by.x = "from", by.y = "id")
      names(edges)[names(edges) == "x"] <- "x0"
      names(edges)[names(edges) == "y"] <- "y0"
      edges <- merge(edges, nodes[, c("id", "x", "y")], by.x = "to", by.y = "id")
      names(edges)[names(edges) == "x"] <- "x1"
      names(edges)[names(edges) == "y"] <- "y1"
    }
    
    currentNode <- clickedNode()
    if (!is.null(currentNode)) {
      highlighted_edges <- edges[edges$from == currentNode | edges$to == currentNode, ]
      normal_edges <- edges[!(edges$from == currentNode | edges$to == currentNode), ]
    } else {
      highlighted_edges <- data.frame()
      normal_edges <- edges
    }
    
    p <- plot_ly(source = "bipartitePlot")
    
    if(nrow(normal_edges) > 0){
      p <- p %>% add_segments(
        data = normal_edges,
        x = ~x0, y = ~y0,
        xend = ~x1, yend = ~y1,
        line = list(color = 'gray', width = ~count / max(adj_filtered$count) * 5),
        hoverinfo = "text",
        hovertemplate = "Edge Count: %{text}",
        text = ~count,
        showlegend = TRUE,
        name = "Edges"
      )
    }
    
    if(nrow(highlighted_edges) > 0){
      p <- p %>% add_segments(
        data = highlighted_edges,
        x = ~x0, y = ~y0,
        xend = ~x1, yend = ~y1,
        line = list(color = 'green', width = ~count / max(adj_filtered$count) * 7),
        hoverinfo = "text",
        hovertemplate = "Edge Count: %{text}",
        text = ~count,
        showlegend = TRUE,
        name = "Highlighted Edges"
      )
    }
    
    nodes$wrapped <- sapply(nodes$id, wrap_label, width = 12)
    
    ai_nodes <- nodes[nodes$type == TRUE, ]
    p <- p %>% add_markers(
      data = ai_nodes,
      x = ~x, y = ~y,
      key = ~id,
      mode = "markers+text",
      text = ~wrapped,
      textposition = "top center",
      marker = list(size = 10, color = 'red'),
      textfont = list(color = 'red'),
      hoverinfo = "text",
      showlegend = TRUE,
      name = "AI Methods"
    )
    
    food_nodes <- nodes[nodes$type == FALSE, ]
    p <- p %>% add_markers(
      data = food_nodes,
      x = ~x, y = ~y,
      key = ~id,
      mode = "markers+text",
      text = ~wrapped,
      textposition = "bottom center",
      marker = list(size = 10, color = 'blue'),
      textfont = list(color = 'blue'),
      hoverinfo = "text",
      showlegend = TRUE,
      name = "Food Subdomains"
    )
    
    p <- p %>% layout(
      title = "Bipartite Network: AI Methods (Top) vs. Food Subdomains (Bottom)",
      xaxis = list(title = "", showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
      yaxis = list(title = "", showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
      legend = list(orientation = "h", x = 0.3, y = -0.1)
    )
    
    return(p)
  })
  
  observeEvent(event_data("plotly_click", source = "bipartitePlot"), {
    ed <- event_data("plotly_click", source = "bipartitePlot")
    if (!is.null(ed) && length(ed$points) > 0 && !is.null(ed$points[[1]]$key)) {
      if (!is.null(clickedNode()) && clickedNode() == ed$points[[1]]$key) {
        clickedNode(NULL)
        message("Cleared clicked node.")
      } else {
        clickedNode(ed$points[[1]]$key)
        message("Clicked node: ", ed$points[[1]]$key)
      }
    }
  })
  
  ####### LDA Topic Modelling ########
  lda_model <- LDA(dtm, k = k, control = list(seed = 1234))
  
  doc_topics <- as.data.frame(posterior(lda_model)$topics)
  colnames(doc_topics) <- paste0("Topic", 1:k)
  
  data_main$AssignedTopic <- apply(doc_topics, 1, function(x) {
    idx <- which.max(x)
    paste("Topic", idx)
  })
  
  for (jj in 1:k) {
    local({
      i <- jj
      output[[paste0("wordcloud_topic_", i)]] <- renderPlot({
        topic_terms <- as.data.frame(table(terms(lda_model, 10)[, i]))
        colnames(topic_terms) <- c("word", "freq")
        
        ggplot(topic_terms, aes(label = word, size = freq)) +
          geom_text_wordcloud_area() +
          scale_size_area(max_size = 15) +
          theme_minimal() +
          ggtitle(paste("Topic", i))
      })
    })
  }
  
  observeEvent(input$selected_topic, {
    filtered_articles <- data_main %>%
      filter(AssignedTopic == input$selected_topic)
    
    if (nrow(filtered_articles) > 0) {
      article_choices <- setNames(filtered_articles$DocID, filtered_articles$TI)
      updateSelectInput(session, "selected_article",
                        choices = article_choices,
                        selected = article_choices[1])
    } else {
      updateSelectInput(session, "selected_article",
                        choices = NULL,
                        selected = NULL)
    }
  })
  
  output$selected_article_details <- renderUI({
    req(input$selected_article)
    doc_id <- input$selected_article
    
    sel_row <- data_main %>% filter(DocID == doc_id)
    
    if (nrow(sel_row) == 0)
      return(HTML("<p>No article selected.</p>"))
    
    HTML(paste0(
      "<div style='white-space: pre-wrap; word-wrap: break-word;'>",
      "<strong>Title:</strong> ", sel_row$TI, "<br><br>",
      "<strong>Abstract:</strong> ", sel_row$AB,
      "</div>"
    ))
  })
  
  output$similar_documents <- DT::renderDataTable({
    req(input$selected_article)
    doc_id <- input$selected_article
    
    top_related_docIDs <- get_top_related(doc_id, top_n = 5)
    related_data <- data_main %>%
      filter(DocID %in% top_related_docIDs) %>%
      mutate(
        TI_short = strtrim(TI, 60),
        AB_short = strtrim(AB, 300)
      )
    
    DT::datatable(
      related_data[, c("TI_short", "AB_short")],
      colnames = c("Title", "Abstract"),
      options = list(pageLength = 5),
      rownames = FALSE
    )
  })
  
  output$commonTermsPlot <- renderPlot({
    term_frequency <- tidy_dtm %>%
      count(term, sort = TRUE)
    
    ggplot(term_frequency[1:20, ], aes(x = reorder(term, n), y = n)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      coord_flip() +
      theme_minimal() +
      ggtitle("Most Common Terms")
  })

}

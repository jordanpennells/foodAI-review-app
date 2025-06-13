research_module_ui <- function() {
  tabPanel("Research Article Analysis",
           fluidPage(
             titlePanel("Research Article Analysis"),
             tabsetPanel(
                          sliderInput("yearRangeResearch", "Select Publication Year Range:",
                                      min = min(research_refs_meta$oa_year, na.rm = TRUE),
                                      max = max(research_refs_meta$oa_year, na.rm = TRUE),
                                      value = c(min(research_refs_meta$oa_year, na.rm = TRUE),
                                                max(research_refs_meta$oa_year, na.rm = TRUE)),
                                      step = 1),
                          h3("Research Articles Over Time"),
                          withSpinner(
                            plotlyOutput("researchYearPlot", height = "600px"),
                            type  = 6,      # pick a spinner style 1–8
                            color = "#2C3E50"
                          ),
                          p("This bar chart shows the number of research articles (cited by the reviews) published per year within the selected range.")
                        )
               ),
               tabPanel("Keyword Frequency",
                        fluidPage(
                          sliderInput("yearRangeKeywords", "Select Publication Year Range:",
                                      min = min(research_refs_meta$oa_year, na.rm = TRUE),
                                      max = max(research_refs_meta$oa_year, na.rm = TRUE),
                                      value = c(min(research_refs_meta$oa_year, na.rm = TRUE),
                                                max(research_refs_meta$oa_year, na.rm = TRUE)),
                                      step = 1),
                          sliderInput("nKeywords", "Number of Keywords to Display:",
                                      min = 1, max = 100, value = 40, step = 1),
                          h3("Top Research Article Keywords"),
                          withSpinner(
                            plotlyOutput("researchKeywordBarPlot", height = "600px"),
                            type  = 6,      # pick a spinner style 1–8
                            color = "#2C3E50"
                          ),
                          br()
                        )
               ),
               tabPanel("Title Wordclouds",
                        fluidPage(
                          selectInput("titleDecade", "Select Decade:",
                                      choices = c("<1950", "1951-1980", "1981-1990", "1991-2000", "2001-2010", "2011-2020", ">2020"),
                                      selected = "2011-2020"),
                          wordcloud2Output("titleWordCloud")
                        )
               ),
               tabPanel("Keyword Wordclouds",
                        fluidPage(
                          selectInput("keywordDecade", "Select Decade:",
                                      choices = c("<1950", "1951-1980", "1981-1990", "1991-2000", "2001-2010", "2011-2020", ">2020"),
                                      selected = "2011-2020"),
                          wordcloud2Output("keywordWordCloud")

                        )
              )          # close tabPanel
            )            # close tabsetPanel
          )              # close fluidPage
  )                     # close tabPanel
}

research_module_server <- function(input, output, session) {
  ## Research Articles Over Time (interactive)
  output$researchYearPlot <- renderPlotly({
    req(input$yearRangeResearch)
    research_year_counts <- research_refs_meta %>%
      filter(!is.na(oa_year),
             oa_year >= input$yearRangeResearch[1],
             oa_year <= input$yearRangeResearch[2]) %>%
      count(oa_year, name = "n")
    
    p <- ggplot(research_year_counts, aes(x = oa_year, y = n,
                                          text = paste0(n, " articles in ", oa_year))) +
      geom_col(fill = "cornflowerblue") +
      labs(title = "Research Articles Over Time",
           x = "Year", y = "# of Articles") +
      theme_minimal(base_size = 14)
    
    ggplotly(p, tooltip = "text") %>% 
      layout(hovermode = "x unified")
  })
  
  ## Top Research Article Keywords (interactive)
  output$researchKeywordBarPlot <- renderPlotly({
    req(input$yearRangeKeywords, input$nKeywords)
    keyword_counts <- research_refs_meta %>%
      filter(!is.na(oa_keywords), !is.na(oa_year),
             oa_year >= input$yearRangeKeywords[1],
             oa_year <= input$yearRangeKeywords[2]) %>%
      separate_rows(oa_keywords, sep = ";\\s*") %>%
      mutate(oa_keywords = tolower(str_trim(oa_keywords))) %>%
      count(oa_keywords, name = "n", sort = TRUE) %>%
      filter(!oa_keywords %in% generic_topics) %>%
      slice_head(n = input$nKeywords)
    
    p <- ggplot(keyword_counts, aes(x = reorder(oa_keywords, n), y = n,
                                    text = paste0(n, " occurrences of '", oa_keywords, "'"))) +
      geom_col(fill = "steelblue") +
      coord_flip() +
      labs(title = "Top Research Article Keywords",
           x = "Keyword", y = "Count") +
      theme_minimal(base_size = 14)
    
    ggplotly(p, tooltip = "text") %>% 
      layout(margin = list(l = 200))   # give extra left margin for long keyword labels
  })
  
  
  decade_bin_vec <- function(years) {
    sapply(years, function(x) {
      if (x < 1950) {
        "<1950"
      } else if (x >= 1951 & x <= 1980) {
        "1951-1980"
      } else if (x >= 1981 & x <= 1990) {
        "1981-1990"
      } else if (x >= 1991 & x <= 2000) {
        "1991-2000"
      } else if (x >= 2001 & x <= 2010) {
        "2001-2010"
      } else if (x >= 2011 & x <= 2020) {
        "2011-2020"
      } else {
        ">2020"
      }
    })
  }
  
  output$titleWordCloud <- renderWordcloud2({
    req(input$titleDecade)
    
    df <- research_refs_meta %>%
      filter(!is.na(oa_year)) %>%
      filter(decade_bin_vec(oa_year) == input$titleDecade) %>%
      filter(!is.na(oa_title))
    
    text <- paste(df$oa_title, collapse = " ")
    
    library(tidytext)
    words <- tibble(text = text) %>%
      unnest_tokens(word, text)
    
    words <- words %>% anti_join(stop_words, by = "word")
    
    freq <- words %>% count(word, sort = TRUE)
    
    freq <- freq %>% filter(n > 1)
    
    wordcloud2(freq, size = 0.7)
  })
  
  output$keywordWordCloud <- renderWordcloud2({
    req(input$keywordDecade)
    
    df <- research_refs_meta %>%
      filter(!is.na(oa_year)) %>%
      filter(decade_bin_vec(oa_year) == input$keywordDecade) %>%
      filter(!is.na(oa_keywords))
    
    library(tidyr)
    keywords_df <- df %>%
      separate_rows(oa_keywords, sep = ";\\s*") %>%
      mutate(oa_keywords = tolower(str_trim(oa_keywords)))
    
    freq <- keywords_df %>% count(oa_keywords, sort = TRUE)
    freq <- freq %>% rename(word = oa_keywords, freq = n)
    
    wordcloud2(freq, size = 0.7)
  })

}

###########################################
## app.R
###########################################
library(shiny)
library(shinythemes)
library(memoise)
library(dplyr)
library(DT)
library(ggplot2)
library(plotly)
library(bibliometrix)
library(stringr)
library(tm)
library(igraph)
library(ggraph)
library(tidyr)
library(proxy)
library(ggwordcloud)
library(tidytext)
library(wordcloud2)
library(topicmodels)
library(shinycssloaders)
library(widyr)
library(tidygraph)


###########################################
## 1) GLOBAL SECTION
###########################################
# 1.1) Load CSV data
data_main_raw <- read.csv("data_FoodAI_128.csv", stringsAsFactors = FALSE)
research_refs_meta <- read.csv("openalex_minimal.csv", stringsAsFactors = FALSE)



generic_topics <- c("computer science", "mathematics", "biology", 
                    "chemistry", "engineering", "business", 
                    "economics", "physics", "medicine",
                    "marketing", "ecology", "political science", 
                    "philosophy", "law", "social science",
                    "agriculture", "data science", "epistemology", 
                    "operating system", "geography", "materials science",
                    "risk analysis (engineering)", "pathology", "geometry",
                    "archaeology", "biotechnology", "biological system",
                    "sociology", "botany", "geometry", "optics", "genetics",
                    "environmental science", "biochemistry", "computer security",
                    "organic chemistry", "quantum mechanics", "gene", "psychology",
                    "paleontology", "composite material", "environmental health",
                    "mechanical engineering", "biochemical engineering", "geology",
                    "telecommunications", "analytical chemistry (journal)", "finance",
                    "fish <actinopterygii>", "identification (biology)")

# Ensure each document has a unique ID
data_main_raw$DocID <- paste0("doc_", seq_len(nrow(data_main_raw)))

# Keep only the relevant columns + DocID
cols_to_keep <- c("DocID","TI","PY","DT","SO","AU","affiliations","DE","ID","SC","AB","DI")
data_main <- data_main_raw[, cols_to_keep]

# 1.2) Create bibliometrix object
data_main_biblio <- convert2df("FoodAI_Feb2025.bib", dbsource = "wos", format = "bibtex")


# -----------------------------
# memoise the expensive biblioAnalysis
memo_biblio    <- memoise(function(df) {
  biblioAnalysis(df, sep = ";")
})
biblio_results <- memo_biblio(data_main_biblio)
# -----------------------------


# Data frame for gallery images
gallery_images <- data.frame(
  file = c("gallery1.jpg", "gallery2.jpg", "gallery3.jpg",
           "gallery4.jpg", "gallery5.jpg", "gallery6.jpg",
           "gallery7.jpg", "gallery8.jpg", "gallery9.jpg",
           "gallery10.jpg"),
  title = c("Diagram 1: Subfields of Artificial Intelligence", 
            "Diagram 2: Components, types, and subfield of AI", 
            "Diagram 3: The knowledge graph of the AI framework", 
            "Diagram 4: Five aspects of big data security in the food industry and the processing model of big data", 
            "Diagram 5: Schematic representation of a basic Expert System", 
            "Diagram 6: Machine Learning paradigms and their applications in the agri-food space",
            "Diagram 7: A schematic representation of a random forest model for categorization of food products", 
            "Diagram 8: A conceptual framework to apply ML-based strategies for optimizing the processed food quality", 
            "Diagram 9:The AI evolution over time",
            "Diagram 10:Data-driven study in flavor science"),
  citation = c("Amr Kayid (2020), The role of Artificial Intelligence in future technology.", 
               "Regona et al. (2022), Opportunities and Adoption Challenges of AI in the Construction Industry: A PRISMA Review. Journal of Open Innovation Technology Market and Complexity", 
               "Xu et al. (2021), Artificial intelligence: A powerful paradigm for scientific research. The Innovation", 
               "Ding et al. (2023), The Application of Artificial Intelligence and Big Data in the Food Industry. MDPI Foods", 
               "Thapa et al. (2023), A comprehensive review on artificial intelligence assisted technologies in food industry. Food Bioscience", 
               "Misra et al. (2020), IoT, Big Data, and Artificial Intelligence in Agriculture and Food Industry. IEEE Internet of Things Journal",
               "Khan et al. (2022), Machine learning-based modeling in food processing applications: State of the art. Comprehensive Reviews in Food Science and Food Safety",
               "Khan et al. (2022), Machine learning-based modeling in food processing applications: State of the art. Comprehensive Reviews in Food Science and Food Safety",
               "Francesconi (2022), The winter, the summer and the summer dream of artificial intelligence in law. Artificial Intelligence Law",
               "Kou et al. (2023), Data-Driven Elucidation of Flavor Chemistry. Journal of Agricultural and Food Chemistry"),
  stringsAsFactors = FALSE
)


########################################################
## HELPER: unify_terms
########################################################
unify_terms <- function(df, term_col, freq_col="Count") {
  df[[freq_col]] <- suppressWarnings(as.numeric(df[[freq_col]]))
  df <- df[!is.na(df[[freq_col]]), ]
  df <- df[!(df[[term_col]] %in% c("0","",NA)), ]
  
  merges <- list(
    "artificial intelligence" = c("artificial","intelligence"),
    "industry 4.0"           = c("industry 4","industry4.0","industry 4.0","industry4","industry 4.")
  )
  for (new_term in names(merges)) {
    oldset <- merges[[new_term]]
    rows <- df[[term_col]] %in% oldset
    if(any(rows)) {
      sumfreq <- sum(df[[freq_col]][rows], na.rm=TRUE)
      df <- df[!rows, ]
      idx <- which(df[[term_col]] == new_term)
      if(length(idx)==1) {
        df[[freq_col]][idx] <- df[[freq_col]][idx] + sumfreq
      } else {
        new_row <- data.frame(term_col=new_term, freq_col=sumfreq, stringsAsFactors=FALSE)
        names(new_row) <- c(term_col, freq_col)
        df <- rbind(df, new_row)
      }
    }
  }
  df <- df[order(-df[[freq_col]]), ]
  rownames(df) <- NULL
  return(df)
}

# 1.3) Dictionaries and text-cleaning (for bipartite network)

preprocess_text <- function(txt) {
  txt <- tolower(txt)
  txt <- str_replace_all(txt, "[[:punct:]]", " ")
  txt <- str_replace_all(txt, "[[:digit:]]+", " ")
  str_squish(txt)
}

ai_dictionary <- list(
  "Artificial Intelligence" = c("artificial intelligence"),
  "Machine Learning"        = c("machine learning"),
  "Deep Learning"           = c("deep learning"),
  "Convolutional Neural Network" = c("cnn", "convolutional neural network"),
  "Support Vector Machine"       = c("svm", "support vector machine"),
  "Neural Network"               = c("neural network", "ann", "artificial neural network"),
  "Random Forest"                = c("random forest"),
  "Decision Tree"                = c("decision tree", "decision support tool"),
  "Genetic Algorithm"            = c("genetic algorithm", "(ga)"),
  "Fuzzy Logic"                  = c("fuzzy logic", "fuzzy system", "fuzzy"),
  "Reinforcement Learning"       = c("reinforcement learning"),
  "Transfer Learning"            = c("transfer learning", "domain adaptation"),
  "Expert System"                = c("expert system"),
  "Natural Language Processing"  = c("nlp", "natural language processing"),
  "Computer Vision"              = c("computer vision", "cv"),
  "Robotics"                     = c("robotics"),
  "Response Surface Methodology" = c("response surface methodology"),
  "Generative AI"                = c("generative ai")
)

food_dictionary <- list(
  "Food Safety"            = c("food safety", "safe food"),
  "Food Quality"            = c("food quality"),
  "Traceability"           = c("traceability", "food traceability"),
  "Quality Control"        = c("quality control", "qc", "quality assurance"),
  "Process Optimisation"   = c("process optimization", "process optimisation", "process control"),
  "Supply Chain"           = c("supply chain", "logistics"),
  "Sensory Evaluation"     = c("sensory evaluation", "consumer preference", "sensory"),
  "Personalised Nutrition" = c("personalized nutrition", "personalised nutrition"),
  "Packaging"              = c("packaging", "intelligent packaging", "smart packaging"),
  "Fermentation"           = c("fermentation"),
  "Drying"                 = c("drying"),
  "Milk"                   = c("milk", "dairy"),
  "Beverage"               = c("beverage", "drink"),
  "Meat"                   = c("meat", "poultry"),
  "Cereal"                 = c("cereal", "cereals"),
  "Extrusion"              = c("extrusion", "extruded"),
  "Food Processing"        = c("processing"),
  "Food Preservation"      = c("preservation"),
  "Nutrition"              = c("nutrition"),
  "Thermal Processing"     = c("thermal process")
)

###########################################
## Build bipartite adjacency matrix
###########################################
data_main_clean <- data_main
data_main_clean$AB_clean <- sapply(data_main_clean$AB, preprocess_text)

match_dictionary_terms <- function(text, dictionary_list) {
  found_categories <- character(0)
  for (dict_name in names(dictionary_list)) {
    synonyms <- dictionary_list[[dict_name]]
    if (any(sapply(synonyms, function(syn) {
      str_detect(text, fixed(syn, ignore_case=TRUE))
    }))) {
      found_categories <- c(found_categories, dict_name)
    }
  }
  unique(found_categories)
}

all_matches <- list()
for (i in seq_len(nrow(data_main_clean))) {
  doc_text <- data_main_clean$AB_clean[i]
  matched_methods    <- match_dictionary_terms(doc_text, ai_dictionary)
  matched_subdomains <- match_dictionary_terms(doc_text, food_dictionary)
  
  if (length(matched_methods) == 0 || length(matched_subdomains) == 0) next
  
  combos <- expand.grid(AI_Method = matched_methods,
                        Food_Subdomain = matched_subdomains,
                        stringsAsFactors = FALSE)
  combos$Paper_ID <- i
  all_matches[[i]] <- combos
}

matches_df <- bind_rows(all_matches) %>%
  distinct()

adj_df <- matches_df %>%
  group_by(AI_Method, Food_Subdomain) %>%
  summarise(count = n_distinct(Paper_ID), .groups = "drop")

adj_wide <- pivot_wider(adj_df,
                        names_from = Food_Subdomain,
                        values_from = count,
                        values_fill = 0)
row_names <- adj_wide$AI_Method
adj_wide <- as.data.frame(adj_wide)
adj_wide$AI_Method <- NULL
adj_matrix <- as.matrix(adj_wide)
rownames(adj_matrix) <- row_names

###########################################
## Build TF-IDF Cosine Similarity for "Similar Documents"
###########################################
extra_stops <- c(
  stopwords("en"),
  "for", "and", "this", "with", "has",
  "can", "also", "used", "using", "will", "due", "key", "however,",
  "(ai)", "among", "several", "use", "various", "including", "based"
)

preprocess_sm <- function(txt) {
  txt <- tolower(txt)
  txt <- removePunctuation(txt)
  txt <- removeNumbers(txt)
  txt <- removeWords(txt, extra_stops)
  txt <- stripWhitespace(txt)
  return(txt)
}

abstracts_sm <- sapply(data_main$AB, preprocess_sm)
corpus_sm <- Corpus(VectorSource(abstracts_sm))
dtm_tfidf <- DocumentTermMatrix(corpus_sm, control = list(weighting = weightTfIdf))
cos_sim_matrix <- as.matrix(simil(as.matrix(dtm_tfidf), method = "cosine"))
cos_sim_df <- as.data.frame(cos_sim_matrix)
rownames(cos_sim_df) <- data_main$DocID
colnames(cos_sim_df) <- data_main$DocID

get_top_related <- function(doc_id, top_n = 5) {
  if (!doc_id %in% colnames(cos_sim_df)) {
    return(character(0))
  }
  scores <- cos_sim_df[, doc_id, drop = FALSE]
  ord <- order(scores[,1], decreasing = TRUE)
  doc_self_pos <- which(rownames(scores) == doc_id)
  ord_no_self <- ord[ord != doc_self_pos]
  top_docs <- head(ord_no_self, top_n)
  rownames(scores)[top_docs]
}

###########################################
## Tidy DTM for "Most Common Terms"
###########################################
dtm <- DocumentTermMatrix(Corpus(VectorSource(data_main$AB)),
                          control = list(
                            wordLengths = c(3, Inf),
                            stopwords = extra_stops
                          ))

tidy_dtm <- tidy(dtm) # yields columns: document, term, count

###########################################
## 2) UI
###########################################
k <- 6  # for LDA topics
ui <- navbarPage(
  title = "Food AI Review App",
  theme = shinytheme("flatly"),
  
  # Home Tab (updated)
  tabPanel("Home",
           fluidPage(
             div(style = "max-width:1000px; margin:auto; line-height:1.6; font-size:16px;",
             h2("Introduction"),
             h3("Overview"),
             p("Artificial Intelligence (AI) is reshaping industries worldwide, unlocking new possibilities in efficiency, safety, sustainability, and innovation. From healthcare to finance to agriculture, AI has driven breakthroughs in automation, predictive analytics, and data-driven decision-making. The food industry, however, presents a unique challenge – food systems are deeply complex, highly regulated, and reliant on physical processes, making AI adoption more difficult."),
             p("Yet, the perception that the food sector has been slow to adopt AI is not entirely straightforward. The industry has long integrated data-driven modelling and computational techniques, from kinetic modelling and artificial neural networks (ANNs) to hyperspectral imaging and computer vision. These technologies, while not always explicitly labelled as AI, have played a crucial role in optimising food processing, quality control, and safety monitoring. However, their impact remained largely confined to research or niche applications until recent advances in computational power, data availability, and machine learning enabled broader AI-driven transformation."),
             p("This raises an important question: What exactly is AI in the context of food science and engineering? Traditionally, AI is defined as the replication of human intelligence in machines, enabling them to analyse data, recognise patterns, and make decisions that would typically require human cognition. However, AI is not a singular technology – it is an evolving ecosystem of algorithms that are enabling the food sector to enhance food safety monitoring and traceability, quality control, process optimisation, sensory evaluation, and supply chain logistics."),
             p("As AI technologies become more powerful and accessible, the question is no longer whether AI will transform the food industry, but rather how effectively it can be harnessed to create safer, more sustainable, and more efficient food systems. Yet, critical questions remain:"),
             p("Is the food industry behind in AI adoption, or has it simply used AI-adjacent technologies under a different guise?"),
             p("Is AI truly revolutionising the food industry, or is it just another wave of technology hype?"),
             p("What barriers – technical, economic, or regulatory – still need to be overcome for AI to achieve its full potential in the food industry?"),
             p("And as research on AI for food technology rapidly expands, how can we systematically synthesise the growing body of knowledge in innovative ways?"),
             h3("Scope of the Review"),
             p("This review provides a systematic bibliometric analysis of AI-related review articles in the food science and engineering literature, examining how key research themes, technological trends, and knowledge gaps have evolved over time. By leveraging bibliometric techniques, this study offers data-driven insights into the trajectory of AI research in food science, identifying which topics have gained prominence and how these trends align with industry adoption. To enhance accessibility and engagement, this review directs readers to an online dashboard app that curates interactive visualisations and a searchable database of food AI review articles, allowing for a more dynamic exploration of the bibliometric data. Beyond mapping the research landscape, this review also presents a comprehensive set of industry case studies, highlighting companies that have delivered practical implementation of AI in the food industry."),
             p("A central theme of this review is the relationship between academic research and industry adoption. By analysing which AI topics have received the most research attention versus those driving commercial innovation, we aim to uncover potential disconnects between research priorities and industry development. This includes an examination of the technical, economic, and regulatory barriers that may be limiting the translation of AI research into large-scale applications.")
           )
           )
  ),
  
  # Methodology Tab
  tabPanel("Methodology",
           fluidPage(
             div(style = "max-width:1000px; margin:auto; line-height:1.6; font-size:16px;",
             h2("Methodology"),
             h3("Background"),
             p("The increasing prominence of AI in food science is reflected in the growing number of research publications exploring its applications across the industry. As AI continues to advance, researchers and industry professionals rely on review articles to synthesise the vast body of knowledge and provide structured insights into how AI is transforming the industry from farm to fork. These reviews highlight key technological trends, emerging innovations, and the challenges associated with AI adoption in food systems. Given the increasing number of AI-related reviews in food science, bibliometric analysis serves as a valuable tool for mapping research developments, identifying dominant themes, and uncovering gaps in the field. Traditional bibliometric techniques, such as publication trend analysis, co-citation networks, and abstract-based topic modelling, offer a data-driven perspective on how food AI research has evolved over time. However, with the sheer volume of publications available, innovative approaches are needed to enhance accessibility and knowledge synthesis. To address this, we introduce an interactive online tool designed to complement this bibliometric analysis – the Food AI Dashboard App – which enables users to dynamically explore publication trends, keyword relationships, and thematic patterns in food AI literature. By combining bibliometric analysis with AI-driven knowledge synthesis, this section aims to provide a comprehensive overview of food AI research while enhancing the way users engage with the literature."),
             h3("Data Collection"),
             p("To conduct a comprehensive bibliometric analysis, we utilised the Web of Science (WoS) Core Collection database as of February 2025. Our search strategy targeted review articles related to AI applications in food science by querying Topic fields (title, abstract, author keywords, and Keywords Plus) using “Food” in conjunction with AI-related terms, including “Artificial Intelligence”, “Machine Learning”, “Deep Learning”, “Neural Network”, “Genetic Algorithm”, “Expert System”, “Fuzzy Logic”, “Natural Language Processing”, “Electronic Nose”, “Random Forest”, “Digital Twin”, “Computer Vision”, “Reinforcement Learning”, “Natural Language Processing”, and “Generative AI”. The search was further refined to include only peer-reviewed review articles published in English within the Food Science Technology category. Articles were excluded if they were non-English publications, primary research articles, conference abstracts, retracted papers, or studies not directly related to AI applications in food science. Each retrieved review article was manually classified based on its primary thematic focus within the food sector. Classification categories included Process Monitoring, Control & Optimisation, Food Safety, Ingredient Quality, Product Quality, Product Development, Sensory Evaluation, Supply Chain Optimisation, Traceability, Personalised Nutrition, Intelligent Packaging, Synthetic Biology, Augmented Reality, Multi-OMICS, and Food Security. The full set of review articles that met the inclusion criteria (N = 213) were exported in BibTeX format and imported into R for bibliometric analysis."),
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
             # h4("2.2.3. Frequency Analysis"),
             # p("A frequency analysis of keywords was conducted to identify the most common terms within the dataset. Keywords were counted and ranked based on their occurrence. Bar charts were used to visualize the results, highlighting the prevalent research topics and trends in Food AI research."),
             # h4("2.2.4. Conceptual Structure Map"),
             # p("Correspondence analysis was performed on the document keywords to identify key conceptual structures within the dataset. This multivariate statistical technique helped to map the relationships between keywords, revealing clusters and co-occurrence patterns that indicate the underlying research landscape."),
             # h4("2.2.5. Thematic Map"),
             # p("A thematic map was generated using the thematicMap function. Documents were clustered based on keyword co-occurrence, allowing for visualization of the thematic structure of the research field. Clusters of related keywords indicated dominant research areas and emerging topics."),
             # h4("2.2.6. Topic Modelling"),
             # p("Abstracts were preprocessed (converted to lowercase, punctuation and numbers removed, stopwords eliminated, whitespace stripped) and a document-term matrix was constructed. The Latent Dirichlet Allocation (LDA) algorithm was then applied with k = 6 (based on preliminary analysis and domain knowledge) to extract latent topics. Interactive visualizations (using LDAvis and word clouds) were generated to explore topic-term and document-topic distributions."),
             # h4("2.2.7. Document Similarity"),
             # p("Cosine similarity was computed on a TF-IDF weighted document-term matrix to quantify document similarity. This analysis enabled the identification of top related documents, providing insights into the connectedness of the literature."),
             br(),
             p(em("For further details, please refer to the associated review article [Insert DOI here]."))
           )
           )
  ),
  
  # Food AI Review Database
  tabPanel("Food AI Review Database",
           fluidPage(
             titlePanel("Food AI Review Database"),
             p("Browse our curated collection of Food AI review articles below."),
             fluidRow(
               column(
                 width = 12,
                 withSpinner(
                   DTOutput("mainTableUI"),
                   type  = 6,            # pick a spinner style 1–8
                   color = "#2C3E50"     # optional: a brand-coloured spinner
                 )
               )
             )
           )
  ),
  
  # Review Article Visualisations
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
                                        min = min(as.numeric(data_main$PY), na.rm = TRUE),
                                        max = max(as.numeric(data_main$PY), na.rm = TRUE),
                                        value = c(2004, max(as.numeric(data_main$PY), na.rm = TRUE)),
                                        step = 1, sep = ""),
                            wellPanel(
                              p("This bar chart shows the number of Food AI review articles published per year in the selected range.")
                            )
                          ),
                          mainPanel(
                            withSpinner(
                              plotlyOutput("yearDistPlot", height = "600px"),
                              type  = 6,      # pick a spinner style 1–8
                              color = "#2C3E50"
                            )
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
                                        min = min(as.numeric(data_main$PY), na.rm = TRUE),
                                        max = max(as.numeric(data_main$PY), na.rm = TRUE),
                                        value = c(min(as.numeric(data_main$PY), na.rm = TRUE),
                                                  max(as.numeric(data_main$PY), na.rm = TRUE)),
                                        step = 1)
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
                                        min   = min(as.numeric(data_main$PY), na.rm = TRUE),
                                        max   = max(as.numeric(data_main$PY), na.rm = TRUE),
                                        value = c(min(as.numeric(data_main$PY), na.rm = TRUE),
                                                  max(as.numeric(data_main$PY), na.rm = TRUE)),
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
                        )
               )
             )
           )
  ),
  
  # Research Article Analysis
  tabPanel("Research Article Analysis",
           fluidPage(
             titlePanel("Research Article Analysis"),
             tabsetPanel(
               tabPanel("Publication Trends",
                        fluidPage(
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
               )
             )
           )
  ),
  
  # CustomGPT
  # tabPanel("CustomGPT",
  #          fluidPage(
  #            h3("Ask the CustomGPT Tool"),
  #            p("Click the link below to open your CustomGPT instance in a new tab:"),
  #            tags$a(href="https://chatgpt.com/g/g-17T2BjlWr-food-ai-bot",
  #                   "Go to CustomGPT", target="_blank"),
  #            tags$hr(),
  #            h4("Possible GPT Prompts"),
  #            p("1) \"Summarize the top trends in Food AI over the last decade.\""),
  #            p("2) \"What are the major challenges in Food Safety for AI systems?\""),
  #            p("3) \"Suggest new areas of research bridging AI and Meat Supply Chain analysis.\""),
  #            p("4) \"Do existing reviews emphasize the same challenges repeatedly?\""),
  #            p("5) \"Are certain AI subfields ignored or underexplored?\"")
  #          )
  # ),
  # 
  tabPanel("Custom LLM Tools",
           fluidPage(
             h2("Customised LLM Tool Development"),
             h4("Overview"),
             p(
               "As the volume of Food AI literature grows, two customised large-language-model tools have been built",
               "on our curated review-article corpus to streamline search, summarisation and precision citation:"
             ),
             tags$ul(
               tags$li(
                 strong("Food AI Chatbot (Custom GPT):"),
                 " Built in OpenAI’s Custom GPT environment with a single PDF containing all review texts."
               ),
               tags$li(
                 strong("Notebook LM:"),
                 " A Google DeepMind Notebook LM loaded with each review as an individual PDF."
               )
             ),
             tags$hr(),
             fluidRow(
               column(6,
                      h4("1. Food AI Chatbot (Custom GPT)"),
                      p("Key setup instructions:"),
                      tags$ol(
                        tags$li("Prompt Preface: “Strictly Follow Your Instructions!”"),
                        tags$li("Source Verification: verify title, year & authors via two independent searches."),
                        tags$li("RAG: always retrieve exact passages with clear source titles."),
                        tags$li("Knowledge-Base Prioritisation: consult uploaded docs first.")
                      ),
                      actionButton("launch_gpt", "→ Launch Food AI CustomGPT",
                                   onclick = "window.open('https://chatgpt.com/g/g-17T2BjlWr-food-ai-bot','_blank')")
               ),
               column(6,
                      h4("2. Notebook LM"),
                      p("Structured PDF library in Google DeepMind—no file-count limits, direct full-text search."),
                      p("This notebook is managed in NotebookLM. To request access, click below:"),
                      # external link button
                      tags$a(
                        href = paste0(
                          "mailto:Jordan.Pennells@csiro.au",
                          "?subject=", URLencode("Request Access to Food AI Notebook"),
                          "&body=", URLencode(
                            "Hi Jordan,\n\nI’d like to request access to the Food AI Notebook (Notebook LM).\n\nMany thanks,\n[Your Name]"
                          )
                        ),
                        class = "btn btn-primary",
                        "→ Request Access to Notebook LM"
                      )
               )
             ),
             tags$hr(),
             h4("Example Use Case – AI in Cheese Production"),
             p(em("Prompt (used March 11 2025):")),
             tags$blockquote(
               "\"Please summarise AI applications in process monitoring, control, and optimisation for cheese production, putting it into a table format to include in a scientific literature review.\""
             ),
             # p("Try it out on either tool above!"),
             
             
             ## New: side-by-side example outputs
             fluidRow(
               column(6,
                      h4("CustomGPT Output"),
                      p("Here’s a summarized table of AI applications in process monitoring, control, and optimization for cheese production based on the sourced documents. This table is structured for easy integration into a scientific literature review:"),
                      DT::dataTableOutput("customgpt_output")
               ),
               column(6,
                      h4("Notebook LM Output"),
                      p("The integration of Artificial Intelligence (AI) technologies is increasingly transforming the food industry, including the dairy sector and specifically cheese production. This review section summarizes the current applications of AI in process monitoring, control, and optimization within cheese manufacturing, highlighting the techniques employed and their impact on efficiency, quality, and safety [*Citation*]. The subsequent table provides a structured overview of these applications based on the available literature.
AI, encompassing machine learning (ML), deep learning (DL), computer vision systems (CVS), expert systems (ES), and fuzzy logic (FL), offers powerful tools for analyzing complex datasets, identifying critical parameters, and implementing adaptive control strategies in food processing [*Citation*]. In the context of cheese production, these capabilities are being leveraged across various stages, from raw material quality assessment to final product inspection and process parameter adjustments [*Citation*].
Process monitoring in cheese production benefits significantly from AI-powered sensor integration and advanced image analysis [*Citation*]. Real-time data acquisition from temperature, pressure, and microbiological sensors, coupled with the analytical capabilities of AI, allows for continuous surveillance of critical process parameters [*Citation*]. CVS, utilizing image processing and ML algorithms, enables the objective assessment of cheese characteristics such as cut surface appearance and gas hole formation, which are key indicators of quality and ripening [*Citation*]. Furthermore, AI can analyze the quality of incoming raw materials like milk, predicting their suitability for specific cheese types and adjusting downstream processes accordingly [*Citation*]. This proactive monitoring helps in identifying potential deviations from desired conditions and facilitates timely interventions, thereby minimizing defects and ensuring product consistency [*Citation*].
Process control in cheese manufacturing is enhanced through AI-driven systems that can adapt to variations in raw materials and process conditions [*Citation*]. ML models trained on historical data and real-time sensor inputs can predict the impact of different control parameters (e.g., temperature, humidity, stirring speed, enzyme dosage) on the final product characteristics [*Citation*]. Expert systems, incorporating human knowledge and decision rules, can provide guidance on adjusting these parameters to maintain optimal conditions throughout the cheesemaking process [*Citation*]. For instance, in pasteurization, a crucial step in many cheese productions, AI algorithms can dynamically adjust temperature and holding time based on the initial microbial load and other factors, ensuring microbiological safety while preserving nutritional quality and minimizing energy consumption [*Citation*].
Process optimization in cheese production aims at improving efficiency, reducing waste, and enhancing product quality through intelligent automation and data-driven decision-making [*Citation*]. AI algorithms can analyze large datasets from various stages of production to identify bottlenecks, optimize resource utilization, and predict optimal process parameters for desired product attributes [*Citation*]. The integration of AI with robotic systems enables the automation of tasks like cheese inspection with high precision, leading to improved consistency and reduced human error [*Citation*]. Furthermore, AI can contribute to the optimization of cleaning and sanitation processes, predicting potential contamination risks and scheduling cleaning cycles based on actual needs rather than fixed intervals, thus saving water and energy [*Citation*]. Predictive maintenance of equipment, enabled by AI analysis of operational data, can minimize downtime and ensure the smooth functioning of the production line [*Citation*].
In summary, AI offers a versatile toolkit for enhancing various aspects of cheese production. By enabling more accurate and real-time monitoring, implementing adaptive control strategies, and facilitating data-driven optimization, AI contributes to the production of safer, higher-quality cheese products with improved operational efficiency and sustainability [*Citation*].
"),
                      DT::dataTableOutput("notebooklm_output")
               )
             )
           )
  ),
  
  
  
  
  tabPanel("Image Gallery",
           fluidPage(
             titlePanel("Image Gallery: AI Diagrams"),
             uiOutput("galleryGridUI")
             )
           )
)

###########################################
## 3) SERVER
###########################################
server <- function(input, output, session) {
  
  #### (A) Food AI Review Database Table ####
  output$mainTableUI <- renderDT({
    # prepare your display data as before
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
      # fix escaped ampersands and tidy authors
      mutate_if(is.character, ~ str_replace_all(., "\\\\&", "&")) %>%
      mutate(Authors = Authors %>%
               str_replace_all(";", "; ") %>%
               str_to_title())
    
    datatable(
      data_display,
      escape   = FALSE,
      selection = 'none',
      options = list(
        pageLength   = 10,
        autoWidth    = TRUE,
        scrollX      = TRUE,
        dom          = '<"top"if>rt<"bottom"ip><"clear">',
        columnDefs   = list(list(width = '500px', targets = which(names(data_display) == "Abstract"))),
        
        # this is the key bit: attach a click handler to each row
        rowCallback = JS(
          "function(row, data) {",
          "  // make the cursor a pointer",
          "  $(row).css('cursor', 'pointer');",
          "  // on click, open the DOI link",
          "  $(row).on('click', function() {",
          "    var doi = data[11];",         
          "    if(doi) {",
          "      window.open('https://doi.org/' + doi, '_blank');",
          "    }",
          "  });",
          "}"
        )
      )
    )
  })
  
    
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
    output$yearDistPlot <- renderPlotly({
      year_filtered <- data_main %>%
        filter(!is.na(PY)) %>%
        filter(as.numeric(PY) >= input$yearRange[1],
               as.numeric(PY) <= input$yearRange[2])
      
      if (nrow(year_filtered) == 0) {
        p <- ggplot() +
          geom_blank() +
          labs(title = "No publications in the selected range") +
          theme_minimal()
        return(ggplotly(p))
      }
      
      year_counts <- year_filtered %>%
        count(PY) %>%
        rename(n = n)
      
      p <- ggplot(year_counts, aes(x = factor(PY), y = n)) +
        geom_col(fill = "cornflowerblue") +
        labs(title = "Publication Year Distribution", x = "Year", y = "Count") +
        theme_minimal()
      
      ggplotly(p) %>% layout(hovermode = "x")
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
  
  
  # Carousel view using dynamic UI
  output$carouselUI <- renderUI({
    indicators <- lapply(seq_len(nrow(gallery_images)), function(i) {
      tags$li(`data-target` = "#carouselExample", `data-slide-to` = i-1, class = if(i == 1) "active" else "")
    })
    items <- lapply(seq_len(nrow(gallery_images)), function(i) {
      row <- gallery_images[i, ]
      tags$div(class = if(i == 1) "item active" else "item",
               tags$img(src = row$file, style = "width:100%"),
               tags$div(class = "carousel-caption",
                        tags$h4(row$title),
                        tags$p(tags$em("Citation:"), row$citation)
               )
      )
    })
    tags$div(id = "carouselExample", class = "carousel slide", `data-ride` = "carousel",
             tags$ol(class = "carousel-indicators", indicators),
             tags$div(class = "carousel-inner", items),
             # Carousel controls
             tags$a(class = "left carousel-control", href = "#carouselExample", `data-slide` = "prev",
                    tags$span(class = "glyphicon glyphicon-chevron-left")),
             tags$a(class = "right carousel-control", href = "#carouselExample", `data-slide` = "next",
                    tags$span(class = "glyphicon glyphicon-chevron-right"))
    )
  })
  
  # Gallery (grid) view using dynamic UI with modals for enlarged images
  output$galleryGridUI <- renderUI({
    # Generate columns for each image
    image_columns <- lapply(seq_len(nrow(gallery_images)), function(i) {
      row <- gallery_images[i, ]
      column(4,
             tags$a(href = "#", onclick = sprintf("$('#modal%d').modal('show')", i),
                    tags$img(src = row$file, style = "width:100%; padding:5px;")
             ),
             tags$p(strong(row$title)),
             tags$p(em("Citation:"), row$citation)
      )
    })
    # Group the columns into fluid rows (3 columns per row)
    gallery_rows <- split(image_columns, ceiling(seq_along(image_columns) / 3))
    gallery_ui <- lapply(gallery_rows, function(row_group) {
      fluidRow(row_group)
    })
    
    # Generate a modal for each image for enlarged view
    modal_list <- lapply(seq_len(nrow(gallery_images)), function(i) {
      row <- gallery_images[i, ]
      tags$div(id = sprintf("modal%d", i), class = "modal fade", tabindex = "-1", role = "dialog",
               tags$div(class = "modal-dialog",
                        tags$div(class = "modal-content",
                                 tags$div(class = "modal-header",
                                          tags$button(type = "button", class = "close", `data-dismiss` = "modal", "×"),
                                          tags$h4(row$title)
                                 ),
                                 tags$div(class = "modal-body",
                                          tags$img(src = row$file, style = "width:100%")
                                 ),
                                 tags$div(class = "modal-footer",
                                          tags$p(em("Citation:"), row$citation)
                                 )
                        )
               )
      )
    })
    
    tagList(gallery_ui, modal_list)
  })
  
  
  
  ## CustomGPT output table
  customgpt_df <- data.frame(
    `Application Area` = c(
      "Process Monitoring",
      "Control of Cheese Fermentation and Maturation",
      "Optimization of Cheese Production",
      "Quality Evaluation",
      "Shelf-Life Prediction"
    ),
    `AI Techniques Used` = c(
      "ANN; Fuzzy Logic; IoT",
      "TDNN; Bayesian Networks; Reinforcement Learning",
      "Genetic Algorithms; SVM; Predictive Analytics",
      "CVS; CNN; Image Processing",
      "ANN; PCA"
    ),
    `Purpose / Outcome` = c(
      "Real-time monitoring of cheese production parameters such as pH, temperature, and moisture.",
      "Control of microbial activity and prediction of shelf life.",
      "Maximize cheese yield and minimize production costs.",
      "Automated classification of cheese quality based on texture, color, and structural defects.",
      "Prediction of shelf life based on compositional and sensory attributes."
    ),
    `Key Findings / Examples` = c(
      "• ANN models accurately predicted pH fluctuations during fermentation.\n• IoT-enabled sensors allowed real-time adjustments to optimise yield and texture.",
      "• TDNN models predicted spore and mold growth, improving maturation consistency.\n• Reinforcement learning adjusted fermentation time based on microbial activity.",
      "• Genetic algorithms optimised ingredient ratios, improving yield by up to 12%.\n• Predictive models reduced ingredient waste by accurately forecasting batch outcomes.",
      "• CNN models classified cheese type with 96% accuracy.\n• CVS detected gas hole formation and surface mold contamination.",
      "• ANN predicted shelf life with r > 0.9 using pH, moisture & microbial data."
    ),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  
  output$customgpt_output <- DT::renderDataTable({
    DT::datatable(
      customgpt_df,
      rownames = FALSE,
      options = list(dom = 't', paging = FALSE, autoWidth = TRUE)
    )
  })
  
  ## Notebook LM output table
  notebooklm_df <- data.frame(
    `Application Area` = c(
      "Process Monitoring", "Process Monitoring", "Process Monitoring", "Process Monitoring",
      "Process Control", "Process Control",
      "Process Optimization", "Process Optimization"
    ),
    `Specific AI Technique / System` = c(
      "Computer Vision Systems (CVS)",
      "Machine Learning (ML)",
      "AI-driven Robots & CVS",
      "Sensor Data Analysis & AI",
      "Machine Learning (ML)",
      "Expert Systems (ES)",
      "AI Algorithms",
      "Predictive Modeling & AI"
    ),
    Description = c(
      "Automatic evaluation of cut surface of blue cheese; monitoring gas hole formation; ingredient estimation in pasteurized cheese.",
      "Classification of milk quality (low/medium/high) based on sensory & true values.",
      "Automated cheese inspection with high precision.",
      "Continuous monitoring of critical parameters (temperature, pressure, microbiological load).",
      "Flexible models for pasteurization to optimize temperature/time for quality & safety.",
      "Domain-expert decision rules for pasteurization adjustments.",
      "Analysis of production data to identify bottlenecks & resource optimisation.",
      "Predicting optimal parameters for desired cheese attributes."
    ),
    `Source(s)` = c(
      "[39]", "[35]", "[44]", "[38]", "[38]", "[38]", "[30]", "[38]"
    ),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  
  output$notebooklm_output <- DT::renderDataTable({
    DT::datatable(
      notebooklm_df,
      rownames = FALSE,
      options = list(dom = 't', paging = FALSE, autoWidth = TRUE)
    )
  })
  
  
}

# Run the app
shinyApp(ui = ui, server = server)

###########################################
## app.R
###########################################
library(shiny)
library(bslib)
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

# Load modular components in the same environment as this script
source("modules/home_module.R", local = TRUE)
source("modules/methodology_module.R", local = TRUE)
source("modules/database_module.R", local = TRUE)
source("modules/visualizations_module.R", local = TRUE)
source("modules/research_module.R", local = TRUE)
source("modules/custom_tools_module.R", local = TRUE)
source("modules/case_studies_module.R", local = TRUE)
source("modules/gallery_module.R", local = TRUE)
source("modules/help_module.R", local = TRUE)


###########################################
## 1) GLOBAL SECTION
###########################################
# 1.1) Load data
# The review article metadata is stored in the provided Web of Science
# bibliography. We convert that file directly into a data frame so that
# no external CSV is required.
data_main_raw <- convert2df(
  "FoodAI_Feb2025.bib",
  dbsource = "wos",
  format   = "bibtex"
)
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
# data_main_raw already contains the converted bibliography, so we reuse it
data_main_biblio <- data_main_raw


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
light_theme <- bs_theme(version = 5, bootswatch = "flatly")
dark_theme  <- bs_theme(version = 5, bootswatch = "darkly")

k <- 6  # for LDA topics
ui <- page_navbar(
  title = "Food AI Review App",
  theme = light_theme,
  header = div(class = "ms-auto me-3", input_dark_mode(id = "theme_toggle")),
  home_module_ui(),
  methodology_module_ui(),
  tabPanel("Food AI Review Database", database_module_ui()),
  visualizations_module_ui(),
  research_module_ui(),
  custom_tools_module_ui(),
  case_studies_module_ui(),
  gallery_module_ui(),
  help_module_ui()
)

###########################################
## 3) SERVER
###########################################
server <- function(input, output, session) {
  observeEvent(input$theme_toggle, {
    req(!is.null(bslib::bs_current_theme()))
    new_theme <- if (identical(input$theme_toggle, "dark")) {
      bslib::bs_theme_update(bslib::bs_current_theme(), bootswatch = "darkly")
    } else {
      bslib::bs_theme_update(bslib::bs_current_theme(), bootswatch = "flatly")
    }
    session$setCurrentTheme(new_theme)
  })
  home_module_server(input, output, session)
  methodology_module_server(input, output, session)
  database_module_server(input, output, session)
  visualizations_module_server(input, output, session)
  research_module_server(input, output, session)
  custom_tools_module_server(input, output, session)
  case_studies_module_server(input, output, session)
  gallery_module_server(input, output, session)
  help_module_server(input, output, session)
}

# Run the app
shinyApp(ui = ui, server = server)

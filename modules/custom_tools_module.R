custom_tools_module_ui <- function() {
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
  
  
  
  
  )
}

custom_tools_module_server <- function(input, output, session) {
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

}

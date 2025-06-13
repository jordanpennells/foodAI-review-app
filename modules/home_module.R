home_module_ui <- function() {
  tabPanel(
    "Home",
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
    )          # close fluidPage
  )            # close tabPanel
}

home_module_server <- function(input, output, session) {
  # No server-side logic for the Home tab
}

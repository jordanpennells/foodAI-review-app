gallery_module_ui <- function() {
  tabPanel("Image Gallery",
           fluidPage(
             titlePanel("Image Gallery: AI Diagrams"),
             uiOutput("galleryGridUI")
           )
  )
}

gallery_module_server <- function(input, output, session) {
  output$carouselUI <- renderUI({
    indicators <- lapply(seq_len(nrow(gallery_images)), function(i) {
      tags$li(`data-target` = "#carouselExample", `data-slide-to` = i-1, class = if(i == 1) "active" else "")
    })
    items <- lapply(seq_len(nrow(gallery_images)), function(i) {
      row <- gallery_images[i, ]
      tags$div(class = if(i == 1) "item active" else "item",
               tags$img(src = row$file, style = "width:100%", alt = row$title),
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
                    tags$img(src = row$file, style = "width:100%; padding:5px;", alt = row$title)
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
                                          tags$img(src = row$file, style = "width:100%", alt = row$title)
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
  
  
  

}

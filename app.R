library(shiny)
library(aprlab05)

all_names <- list_pokemon()

# ---------- 稀有度颜色 ----------
rarity_color <- function(r) {
  switch(as.character(r),
         "Common"    = "#4CAF50",
         "Uncommon"  = "#2196F3",
         "Rare"      = "#9C27B0",
         "Very Rare" = "#FF9800",
         "Legendary" = "#F44336",
         "Mythical"  = "#E91E63",
         "#000000"
  )
}

# ---------- 类型颜色 ----------
type_colors <- c(
  normal   = "#A8A77A", fire     = "#EE8130",
  water    = "#6390F0", electric = "#F7D02C",
  grass    = "#7AC74C", ice      = "#96D9D6",
  fighting = "#C22E28", poison   = "#A33EA1",
  ground   = "#E2BF65", flying   = "#A98FF3",
  psychic  = "#F95587", bug      = "#A6B91A",
  rock     = "#B6A136", ghost    = "#735797",
  dragon   = "#6F35FC", dark     = "#705746",
  steel    = "#B7B7CE", fairy    = "#D685AD"
)

# ---------- species 卡片 ----------
render_species_info <- function(name) {
  info <- get_species(name)[1, , drop = FALSE]
  
  div(
    style = "margin-top: 10px; padding: 10px;
             border: 1px solid #ddd; border-radius: 8px;
             background: #fafafa; text-align: center;",
    tags$p(
      style = "font-size: 18px; font-weight: bold;",
      "Rarity: ",
      tags$span(style = paste0("color:", rarity_color(info$rarity_level), ";"),
                info$rarity_level)
    ),
    tags$p(
      style = "font-size: 16px;",
      "Capture rate: ", tags$b(info$capture_rate)
    ),
    tags$p(
      style = "font-size: 16px;",
      "Growth rate: ", tags$b(info$growth_rate)
    )
  )
}

# ---------- type 卡片 ----------
render_types_info <- function(types) {
  badges <- lapply(types, function(t) {
    col <- type_colors[[t]]
    if (is.null(col)) col <- "#888888"
    tags$span(
      style = paste0(
        "display: inline-block;",
        "margin: 2px;",
        "padding: 4px 12px;",
        "border-radius: 12px;",
        "background:", col, ";",
        "color: white;",
        "font-weight: bold;",
        "font-size: 14px;",
        "text-transform: capitalize;"
      ),
      t
    )
  })
  
  div(
    style = "margin-top: 10px; padding: 10px;
             border: 1px solid #ddd; border-radius: 8px;
             background: #fafafa; text-align: center;",
    tags$p(
      style = "font-size: 16px; font-weight: bold; margin-bottom: 6px;",
      "Type"
    ),
    badges
  )
}

# ---------- UI ----------
ui <- fluidPage(
  h1("❤️❤️ Choose Your Pokémon ❤️❤️" ,
  style = "text-align: center; color: #E91E63;"),
  
  sidebarLayout(
    sidebarPanel(
      width = 2,
      selectInput("first",  "First Pokémon",  choices = all_names, selected = "fuecoco"),
      selectInput("second", "Second Pokémon", choices = all_names, selected = "pikachu")
    ),
    mainPanel(
      fluidRow(
        column(
          width = 6,
          div(style = "text-align: center;",
              plotOutput("image_first", height = "250px")),
          plotOutput("plot_first"),
          uiOutput("species_first"),
          uiOutput("types_first")
        ),
        column(
          width = 6,
          div(style = "text-align: center;",
              plotOutput("image_second", height = "250px")),
          plotOutput("plot_second"),
          uiOutput("species_second"),
          uiOutput("types_second")
        )
      ),
      tableOutput("stats"),
      tableOutput("types")
    )
  )
)

# ---------- Server ----------
server <- function(input, output, session) {
  
  first <- reactive({
    req(input$first)
    pokemon$new(input$first)
  })
  
  second <- reactive({
    req(input$second)
    pokemon$new(input$second)
  })
  
  output$image_first <- renderPlot({
    req(first())
    if (is.na(first()$sprite)) return(NULL)
    first()$show_image()
  })
  
  output$image_second <- renderPlot({
    req(second())
    if (is.na(second()$sprite)) return(NULL)
    second()$show_image()
  })
  
  output$plot_first  <- renderPlot(first()$plot_stats())
  output$plot_second <- renderPlot(second()$plot_stats())
  
  output$stats <- renderTable({
    stat_comparison(first(), second())
  })
  
  output$types <- renderTable({
    type_matchup(first(), second())
  })
  
  output$species_first <- renderUI({
    req(input$first)
    render_species_info(input$first)
  })
  
  output$species_second <- renderUI({
    req(input$second)
    render_species_info(input$second)
  })
  
  output$types_first <- renderUI({
    req(first())
    render_types_info(first()$types)
  })
  
  output$types_second <- renderUI({
    req(second())
    render_types_info(second()$types)
  })
}

shinyApp(ui, server)
library(shiny)
library(aprlab05)

# Gets all Pokémon names for the menus
all_names <- list_pokemon()

# ---------- 稀有度颜色 ----------
# Returns the color of a rarity level
rarity_color <- function(r) {
  switch(
    as.character(r),
    "Common" = "#4CAF50",
    "Uncommon" = "#2196F3",
    "Rare" = "#9C27B0",
    "Very Rare" = "#FF9800",
    "Legendary" = "#F44336",
    "Mythical" = "#E91E63",
    "#000000"
  )
}

# ---------- 类型颜色 ----------
# Sets the color of each type
type_colors <- c(
  normal = "#A8A77A",
  fire = "#EE8130",
  water = "#6390F0",
  electric = "#F7D02C",
  grass = "#7AC74C",
  ice = "#96D9D6",
  fighting = "#C22E28",
  poison = "#A33EA1",
  ground = "#E2BF65",
  flying = "#A98FF3",
  psychic = "#F95587",
  bug = "#A6B91A",
  rock = "#B6A136",
  ghost = "#735797",
  dragon = "#6F35FC",
  dark = "#705746",
  steel = "#B7B7CE",
  fairy = "#D685AD"
)

# ---------- species 卡片 ----------
# Builds the Rarity box
render_species_info <- function(name) {
  # Gets the species data
  info <- get_species(name)[1, , drop = FALSE]

  div(
    style = "margin-top: 10px; padding: 10px;
             border: 1px solid #ddd; border-radius: 8px;
             background: #fafafa; text-align: center;",
    tags$p(
      style = "font-size: 18px; font-weight: bold;",
      "Rarity: ",
      tags$span(
        style = paste0("color:", rarity_color(info$rarity_level), ";"),
        info$rarity_level
      )
    ),
    tags$p(
      style = "font-size: 16px;",
      "Capture rate: ",
      tags$b(info$capture_rate)
    ),
    tags$p(
      style = "font-size: 16px;",
      "Growth rate: ",
      tags$b(info$growth_rate)
    )
  )
}

# ---------- type 卡片 ----------
# Builds the Type box
render_types_info <- function(p) {
  # Creates a colored badge for each type
  badges <- lapply(p$types, function(t) {
    col <- type_colors[[t]]
    if (is.null(col)) {
      col <- "#888888"
    }
    tags$span(
      style = paste0(
        "display: inline-block;",
        "margin: 2px;",
        "padding: 4px 12px;",
        "border-radius: 12px;",
        "background:",
        col,
        ";",
        "color: white;",
        "font-weight: bold;",
        "font-size: 14px;",
        "text-transform: capitalize;"
      ),
      t
    )
  })

  item_style <- "display: inline-block; margin: 0 8px;"

  div(
    style = "margin-top: 10px; margin-bottom: 10px; padding: 10px;
             border: 1px solid #ddd; border-radius: 8px;
             background: #fafafa; text-align: center; flex: 1;",
    # First line: ID on the left, types on the right
    div(
      style = "display: flex; justify-content: space-between;
               align-items: center; font-size: 16px; font-weight: bold;",
      tags$span(paste0("#", p$id)),
      tags$span("Type", badges)
    ),
    # Second line: abilities, height and weight in SI units
    tags$p(
      style = "font-size: 14px; margin-top: 8px; margin-bottom: 0;",
      tags$span(
        style = item_style,
        "Abilities: ",
        tags$b(
          style = "text-transform: capitalize;",
          paste(p$abilities, collapse = ", ")
        )
      ),
      tags$span(
        style = item_style,
        "Height: ",
        tags$b(format(p$height / 10, nsmall = 1), " m")
      ),
      tags$span(
        style = item_style,
        "Weight: ",
        tags$b(format(p$weight / 10, nsmall = 1), " kg")
      )
    )
  )
}

# ---------- matchup 卡片 ----------
# Sets the labels of the stats to compare
stat_labels <- c(
  attack = "Attack",
  defense = "Defense",
  special_attack = "Sp. Atk",
  special_defense = "Sp. Def"
)

# Capitalizes the first letter of a name
cap_name <- function(name) {
  paste0(toupper(substring(name, 1, 1)), substring(name, 2))
}

# Returns green if the left Pokémon wins, red if the right one, grey if even
advantage_color <- function(attack, defense, to_right) {
  if (attack == defense) {
    return("#888888")
  }
  attacker_wins <- attack > defense
  left_wins <- if (to_right) attacker_wins else !attacker_wins
  if (left_wins) "#4FAE80" else "#DB6B77"
}

# Describes a type multiplier in words
effect_label <- function(multiplier) {
  if (multiplier == 0) {
    "no effect"
  } else if (multiplier < 1) {
    "not very effective"
  } else if (multiplier > 1) {
    "super effective"
  } else {
    "normal"
  }
}

# Builds one line: left side, colored arrow, right side
matchup_row <- function(
  attacker_side,
  text,
  defender_side,
  color,
  to_right,
  small = FALSE
) {
  # Places the attacker on the left or on the right
  if (to_right) {
    left <- attacker_side
    arrow <- paste(text, "→")
    right <- defender_side
  } else {
    left <- defender_side
    arrow <- paste("←", text)
    right <- attacker_side
  }
  # Makes the stat lines smaller
  side_size <- if (small) "12px" else "16px"
  arrow_size <- if (small) "11px" else "14px"
  padding <- if (small) "1px 8px" else "4px 12px"
  side_style <- paste0(
    "display: inline-block; width: 30%; font-size: ",
    side_size,
    ";"
  )

  div(
    style = "margin: 2px 0;",
    tags$span(style = paste(side_style, "text-align: right;"), left),
    tags$span(
      style = "display: inline-block; width: 36%;",
      tags$span(
        style = paste0(
          "display: inline-block;",
          "padding: ",
          padding,
          ";",
          "border-radius: 12px;",
          "background:",
          color,
          ";",
          "color: white;",
          "font-weight: bold;",
          "font-size: ",
          arrow_size,
          ";"
        ),
        arrow
      )
    ),
    tags$span(style = paste(side_style, "text-align: left;"), right)
  )
}

# Compares an attack stat against a defense stat
stat_row <- function(attacker, defender, attack_stat, defense_stat, to_right) {
  attack <- attacker$stats[[attack_stat]]
  defense <- defender$stats[[defense_stat]]

  # Says who is stronger
  text <- if (attack > defense) {
    paste(cap_name(attacker$name), "is stronger")
  } else if (attack < defense) {
    paste(cap_name(defender$name), "is stronger")
  } else {
    "Even"
  }

  matchup_row(
    paste(stat_labels[[attack_stat]], attack),
    text,
    paste(stat_labels[[defense_stat]], defense),
    advantage_color(attack, defense, to_right),
    to_right,
    small = TRUE
  )
}

# Builds the three lines of one Pokémon attacking the other
matchup_block <- function(attacker, defender, multiplier, to_right) {
  div(
    style = "margin-bottom: 8px;",
    matchup_row(
      tags$b(paste(cap_name(attacker$name), "attacks")),
      paste0("×", multiplier, " ", effect_label(multiplier)),
      tags$b(paste(cap_name(defender$name), "defends")),
      advantage_color(multiplier, 1, to_right),
      to_right
    ),
    stat_row(attacker, defender, "attack", "defense", to_right),
    stat_row(attacker, defender, "special_attack", "special_defense", to_right)
  )
}

# Builds the Matchup box
render_matchup_info <- function(p1, p2, first_attacks) {
  # Gets the type multipliers of both Pokémon
  matchup <- type_matchup(p1$name, p2$name)

  # Keeps only the half chosen with the Switch button
  block <- if (first_attacks) {
    matchup_block(p1, p2, matchup$attack, TRUE)
  } else {
    matchup_block(p2, p1, matchup$defense, FALSE)
  }

  # Decides who has the type advantage
  verdict <- if (matchup$attack > matchup$defense) {
    paste(cap_name(p1$name), "has the type advantage")
  } else if (matchup$attack < matchup$defense) {
    paste(cap_name(p2$name), "has the type advantage")
  } else {
    "No type advantage"
  }

  div(
    style = "margin-top: 10px; margin-bottom: 10px; padding: 6px 10px;
             border: 1px solid #ddd; border-radius: 8px;
             background: #fafafa; text-align: center;",
    tags$p(
      style = "font-size: 16px; font-weight: bold; margin-bottom: 2px;",
      "Matchup"
    ),
    block,
    tags$p(
      style = "font-size: 18px; font-weight: bold; margin-bottom: 0;",
      verdict
    )
  )
}

# ---------- UI ----------
ui <- fluidPage(
  h1(
    "❤️❤️ Choose Your Pokémon ❤️❤️",
    style = "text-align: center; color: #E91E63;"
  ),

  sidebarLayout(
    # Menus to choose both Pokémon
    sidebarPanel(
      width = 2,
      selectInput(
        "first",
        "First Pokémon",
        choices = all_names,
        selected = "fuecoco"
      ),
      selectInput(
        "second",
        "Second Pokémon",
        choices = all_names,
        selected = "pikachu"
      )
    ),
    mainPanel(
      # Pictures
      fluidRow(
        column(
          width = 6,
          div(
            style = "text-align: center;",
            plotOutput("image_first", height = "250px")
          )
        ),
        column(
          width = 6,
          div(
            style = "text-align: center;",
            plotOutput("image_second", height = "250px")
          )
        )
      ),
      # Type boxes
      uiOutput("types"),
      # Stat charts and Rarity boxes
      fluidRow(
        column(
          width = 6,
          plotOutput("plot_first"),
          uiOutput("species_first")
        ),
        column(
          width = 6,
          plotOutput("plot_second"),
          uiOutput("species_second")
        )
      ),
      # Matchup box with the Switch button over its corner
      div(
        style = "position: relative;",
        actionButton(
          "switch",
          "Switch",
          class = "btn-xs",
          style = "position: absolute; left: 10px; top: 16px;"
        ),
        uiOutput("matchup")
      )
    )
  )
)

# ---------- Server ----------
server <- function(input, output, session) {
  # Creates each Pokémon only when its menu changes
  first <- reactive({
    req(input$first)
    pokemon$new(input$first)
  })

  second <- reactive({
    req(input$second)
    pokemon$new(input$second)
  })

  # Draws the pictures, if the Pokémon has one
  output$image_first <- renderPlot({
    req(first())
    if (is.na(first()$image)) {
      return(NULL)
    }
    first()$show_image()
  })

  output$image_second <- renderPlot({
    req(second())
    if (is.na(second()$image)) {
      return(NULL)
    }
    second()$show_image()
  })

  # Plots the base stats
  output$plot_first <- renderPlot(first()$plot_stats())
  output$plot_second <- renderPlot(second()$plot_stats())

  # Shows one half of the matchup, switching on every click
  output$matchup <- renderUI({
    req(first(), second())
    render_matchup_info(first(), second(), input$switch %% 2 == 0)
  })

  # Uses the species name so alternate forms work
  output$species_first <- renderUI({
    req(input$first)
    render_species_info(first()$species)
  })

  output$species_second <- renderUI({
    req(input$second)
    render_species_info(second()$species)
  })

  # Builds both Type boxes in one row so they have the same height
  output$types <- renderUI({
    req(first(), second())
    div(
      style = "display: flex; gap: 30px;",
      render_types_info(first()),
      render_types_info(second())
    )
  })
}

shinyApp(ui, server)

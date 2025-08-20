#Global Ecosystem Mapping

# =========================
#  INSTALL & LOAD PACKAGES
# =========================
options(repos = c(CRAN = "https://cloud.r-project.org/"))

pkgs <- c(
  "shiny","shinydashboard","readxl","dplyr","tidyr","plotly"
)
to_install <- setdiff(pkgs, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, dependencies = TRUE)
invisible(lapply(pkgs, library, character.only = TRUE))

# Small helper
`%||%` <- function(x, y) if (!is.null(x)) x else y

# Normalise any label to a likely dummy column name in your sheet
to_col <- function(s) {
  s <- iconv(s, to = "ASCII//TRANSLIT")
  s <- gsub("[^A-Za-z0-9]+", "_", s)
  s <- gsub("_+", "_", s)
  s <- gsub("^_|_$", "", s)
  s
}

# =========================
#  LOAD DATA
# =========================
# Adjust path
data <- readxl::read_excel("data/analysis.xlsx", sheet = 1)

# Clean column names
names(data) <- str_replace_all(names(data), "\\s+", "_")

# Trim text cols
trim_all <- function(x) {
  x <- gsub("\u00A0", " ", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}
data <- data %>% mutate(across(where(is.character), trim_all))

# Make sure key columns exist
key_cols <- c("Organization","Category")
for (nm in key_cols) if (!nm %in% names(data)) data[[nm]] <- NA

# Clean Category (remove numbering prefixes like "6. ...")
clean_category <- function(x) trim_all(gsub("^\\s*\\d+\\.?\\s*", "", x))
data <- data %>% mutate(Category_clean = clean_category(Category))

# =========================
#  DEFINITIONS & COLOR MAPS
# =========================
category_defs <- list(
  "Global Civil Society and Grassroots Networks" =
    "Includes NGOs, community-based organizations, informal movements, innovation networks, humanitarian networks.",
  "Social and Solidarity Economy Actors" =
    "Includes social enterprises, cooperatives, mutuals, platform cooperatives, new economy movements.",
  "Inclusive and Impact Oriented Finance" =
    "Includes microfinance institutions, inclusive finance providers, impact investors, blended finance platforms.",
  "Corporate Social Responsibility and Private Sector Engagement" =
    "Includes CSR programs, ESG reporting platforms, social procurement alliances, corporate philanthropy.",
  "Knowledge and Narrative Infrastructure" =
    "Includes think tanks, academic institutes, research collaboratives, civic media, storytelling platforms.",
  "Ecosystem Builders and Connectors" =
    "Includes incubators, innovation hubs, conveners, strategic consultancies, ecosystem platforms.",
  "Multilateral and Bilateral Development Partners" =
    "Includes UN agencies, multilateral banks, bilateral donors, intergovernmental cooperation platforms.",
  "Foundations" =
    "Includes church funds, zakat, wealth advisors, donor advised funds, community foundations.",
  "Large International NGOs" =
    "Includes major global NGOs with broad thematic and geographic coverage."
)


# =========================
#  UI
# =========================
ui <- dashboardPage(
  dashboardHeader(title = "Global Actors of Social Innovation", titleWidth = 420),
  dashboardSidebar(width = 200,
                   sidebarMenu(id = "tabs",
                               menuItem("Overview", tabName = "overview", icon = icon("home"))
                   )
  ),
  dashboardBody(
    tabItems(
      # 1) OVERVIEW
      tabItem(tabName = "overview",
              fluidRow(
                valueBoxOutput("totalActors", width = 4),
                valueBoxOutput("totalCategories", width = 4)
              ),
              fluidRow(
                box(title = "Actors by Category", width = 9, plotlyOutput("catPlot"))
              ),
              fluidRow(
                box(title = "Category Definitions", width = 10, uiOutput("catDefs"))
              )
      )
    )
  )
)

# =========================
#  SERVER
# =========================
server <- function(input, output, session) {
  
  # ---- KPI
  output$totalActors <- renderValueBox({
    valueBox(nrow(data), "Total Actors", icon = icon("users"), color = "olive")
  })
  output$totalCategories <- renderValueBox({
    valueBox(9, "Total Categories", icon = icon("list"), color = "light-blue")
  })
  
  # ---- Overview: category bar 
  output$catPlot <- renderPlotly({
    df <- data %>% count(Category_clean, name = "n") %>%
      mutate(Category_clean = forcats::fct_reorder(Category_clean, n, .desc = TRUE))
    p <- ggplot(df, aes(x = Category_clean, y = n, fill = Category_clean)) +
      geom_col() +
      geom_text(aes(label = n), vjust = -0.25, size = 4) +
      labs(x = "Category", y = "Number of actors") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 25, hjust = 1))
    gp <- ggplotly(p, tooltip = NULL) %>% 
      layout(showlegend = FALSE) %>% 
      config(displayModeBar = FALSE)
    for (i in seq_along(gp$x$data)) gp$x$data[[i]]$hoverinfo <- "skip"
    gp
  })
  
  output$catDefs <- renderUI({
    tagList(lapply(names(category_defs), function(cat) {
      div(tags$b(cat), ": ", category_defs[[cat]], tags$br(), tags$br())
    }))
  })
}  
  # =========================
  #  FILTERING (wide data, dummies)
  # =========================
  filtered_data <- reactive({
    df <- data
    
    # Category
    if (length(input$filterCategory)) {
      df <- df %>% filter(Category_clean %in% input$filterCategory)
    }
    
  })

# =========================
#  RUN APP
# =========================
#shinyApp(ui, server)
shinyApp(ui = ui, server = server)
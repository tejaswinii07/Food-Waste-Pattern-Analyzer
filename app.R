# ==========================================
# FOOD WASTE PATTERN ANALYZER
# R Shiny Dashboard
# ==========================================

# 1. Load required libraries
library(shiny)
library(ggplot2)
library(DT)

# 2. Load the dataset
if (!file.exists("food_waste_data.csv")) {
  stop("Error: food_waste_data.csv not found. Keep it in the same folder as app.R.")
}

# Load the CSV using its full file path
csv_path <- "C:/Users/DELL/OneDrive/Documents/Food_Waste_Analyzer/food_waste_data.csv"

food_data <- read.csv(
  csv_path,
  stringsAsFactors = FALSE,
  strip.white = TRUE
)

# Check that the data loaded
print(dim(food_data))
print(names(food_data))
print(head(food_data))

# 3. Validate dataset columns
required_columns <- c(
  "Date",
  "Food_Item",
  "Prepared_kg",
  "Served_kg",
  "Consumed_kg"
)

missing_columns <- setdiff(
  required_columns,
  names(food_data)
)

if (length(missing_columns) > 0) {
  stop(
    paste(
      "Missing columns:",
      paste(missing_columns, collapse = ", ")
    )
  )
}

# 4. Prepare and clean data
food_data$Date <- as.Date(
  food_data$Date,
  format = "%m/%d/%Y"
)

food_data$Prepared_kg <- as.numeric(food_data$Prepared_kg)
food_data$Served_kg <- as.numeric(food_data$Served_kg)
food_data$Consumed_kg <- as.numeric(food_data$Consumed_kg)

# 5. Calculate food waste
food_data$Unserved_kg <- pmax(
  food_data$Prepared_kg - food_data$Served_kg,
  0
)

food_data$Plate_Waste_kg <- pmax(
  food_data$Served_kg - food_data$Consumed_kg,
  0
)

food_data$Total_Waste_kg <- pmax(
  food_data$Prepared_kg - food_data$Consumed_kg,
  0
)

food_data$Waste_Percentage <- ifelse(
  food_data$Prepared_kg > 0,
  food_data$Total_Waste_kg / food_data$Prepared_kg * 100,
  0
)

# ==========================================
# 6. USER INTERFACE
# ==========================================

ui <- fluidPage(
  
  # Dashboard styling
  tags$head(
    tags$title("Food Waste Pattern Analyzer"),
    
    tags$style(HTML("
      body {
        background-color: #f4f7fb;
        font-family: Arial, sans-serif;
      }

      .main-title {
        color: #174a3a;
        font-weight: bold;
        margin-top: 20px;
      }

      .subtitle {
        color: #52616b;
        margin-bottom: 25px;
      }

      .metric-card {
        background-color: white;
        padding: 18px;
        border-radius: 12px;
        border: 1px solid #dce5e0;
        margin-bottom: 15px;
        min-height: 105px;
        text-align: center;
      }

      .metric-value {
        font-size: 26px;
        font-weight: bold;
        color: #176b50;
      }

      .metric-label {
        font-size: 14px;
        color: #555555;
        margin-top: 8px;
      }

      .panel {
        background-color: white;
        padding: 16px;
        border-radius: 12px;
        margin-bottom: 18px;
        border: 1px solid #e0e5e9;
      }

      h4 {
        color: #174a3a;
        font-weight: bold;
      }
    "))
  ),
  
  # Dashboard heading
  h1(
    "Food Waste Pattern Analyzer",
    class = "main-title"
  ),
  
  p(
    "Monitor food preparation, consumption, and waste patterns.",
    class = "subtitle"
  ),
  
  # Main layout
  sidebarLayout(
    
    # ======================================
    # SIDEBAR
    # ======================================
    
    sidebarPanel(
      
      h4("Filter Data"),
      
      dateRangeInput(
        inputId = "date_range",
        label = "Select Date Range",
        start = min(food_data$Date, na.rm = TRUE),
        end = max(food_data$Date, na.rm = TRUE),
        min = min(food_data$Date, na.rm = TRUE),
        max = max(food_data$Date, na.rm = TRUE),
        format = "yyyy-mm-dd"
      ),
      
      selectInput(
        inputId = "food_item",
        label = "Select Food Item",
        choices = c(
          "All items",
          sort(unique(food_data$Food_Item))
        ),
        selected = "All items"
      ),
      
      actionButton(
        inputId = "reset_filters",
        label = "Reset Filters"
      ),
      
      hr(),
      
      p("Total Waste = Prepared Food - Consumed Food"),
      
      p("All quantities are measured in kilograms.")
    ),
    
    # ======================================
    # DASHBOARD CONTENT
    # ======================================
    
    mainPanel(
      
      # Summary cards
      fluidRow(
        
        column(
          width = 3,
          
          div(
            class = "metric-card",
            
            div(
              class = "metric-value",
              textOutput("prepared_total", inline = TRUE)
            ),
            
            div(
              class = "metric-label",
              "Total Prepared (kg)"
            )
          )
        ),
        
        column(
          width = 3,
          
          div(
            class = "metric-card",
            
            div(
              class = "metric-value",
              textOutput("consumed_total", inline = TRUE)
            ),
            
            div(
              class = "metric-label",
              "Total Consumed (kg)"
            )
          )
        ),
        
        column(
          width = 3,
          
          div(
            class = "metric-card",
            
            div(
              class = "metric-value",
              textOutput("waste_total", inline = TRUE)
            ),
            
            div(
              class = "metric-label",
              "Total Waste (kg)"
            )
          )
        ),
        
        column(
          width = 3,
          
          div(
            class = "metric-card",
            
            div(
              class = "metric-value",
              textOutput("waste_percent", inline = TRUE)
            ),
            
            div(
              class = "metric-label",
              "Waste Percentage"
            )
          )
        )
      ),
      
      # Charts
      fluidRow(
        
        column(
          width = 6,
          
          div(
            class = "panel",
            
            h4("Food Waste by Item"),
            
            plotOutput(
              "waste_plot",
              height = "300px"
            )
          )
        ),
        
        column(
          width = 6,
          
          div(
            class = "panel",
            
            h4("Daily Prepared vs Consumed"),
            
            plotOutput(
              "daily_plot",
              height = "300px"
            )
          )
        )
      ),
      
      # Data table
      div(
        class = "panel",
        
        h4("Food Waste Data"),
        
        downloadButton(
          "download_data",
          "Download CSV"
        ),
        
        br(),
        br(),
        
        DTOutput("data_table")
      ),
      
      # Recommendations
      div(
        class = "panel",
        
        h4("Recommendations"),
        
        uiOutput("recommendations")
      )
    )
  )
)

# ==========================================
# 7. SERVER LOGIC
# ==========================================

server <- function(input, output, session) {
  
  # Reset filters
  observeEvent(input$reset_filters, {
    
    updateDateRangeInput(
      session,
      "date_range",
      start = min(food_data$Date, na.rm = TRUE),
      end = max(food_data$Date, na.rm = TRUE)
    )
    
    updateSelectInput(
      session,
      "food_item",
      selected = "All items"
    )
  })
  
  # Filter dataset
  filtered_data <- reactive({
    
    data <- food_data
    
    if (
      !is.null(input$date_range) &&
      length(input$date_range) == 2
    ) {
      
      data <- data[
        data$Date >= input$date_range[1] &
          data$Date <= input$date_range[2],
        ,
        drop = FALSE
      ]
    }
    
    if (
      !is.null(input$food_item) &&
      input$food_item != "All items"
    ) {
      
      data <- data[
        data$Food_Item == input$food_item,
        ,
        drop = FALSE
      ]
    }
    
    data
  })
  
  # ======================================
  # SUMMARY CARDS
  # ======================================
  
  output$prepared_total <- renderText({
    
    sprintf(
      "%.2f",
      sum(filtered_data()$Prepared_kg, na.rm = TRUE)
    )
  })
  
  output$consumed_total <- renderText({
    
    sprintf(
      "%.2f",
      sum(filtered_data()$Consumed_kg, na.rm = TRUE)
    )
  })
  
  output$waste_total <- renderText({
    
    sprintf(
      "%.2f",
      sum(filtered_data()$Total_Waste_kg, na.rm = TRUE)
    )
  })
  
  output$waste_percent <- renderText({
    
    data <- filtered_data()
    
    total_prepared <- sum(
      data$Prepared_kg,
      na.rm = TRUE
    )
    
    total_waste <- sum(
      data$Total_Waste_kg,
      na.rm = TRUE
    )
    
    if (total_prepared <= 0) {
      return("0.00%")
    }
    
    sprintf(
      "%.2f%%",
      total_waste / total_prepared * 100
    )
  })
  
  # ======================================
  # CHART 1: WASTE BY FOOD ITEM
  # ======================================
  
  output$waste_plot <- renderPlot({
    
    data <- filtered_data()
    
    validate(
      need(
        nrow(data) > 0,
        "No data matches the selected filters."
      )
    )
    
    waste_summary <- aggregate(
      Total_Waste_kg ~ Food_Item,
      data = data,
      FUN = sum
    )
    
    ggplot(
      waste_summary,
      aes(
        x = reorder(Food_Item, Total_Waste_kg),
        y = Total_Waste_kg
      )
    ) +
      
      geom_col() +
      
      coord_flip() +
      
      labs(
        title = "Total Waste by Food Item",
        x = "Food Item",
        y = "Waste (kg)"
      ) +
      
      theme_minimal(base_size = 12)
  })
  
  # ======================================
  # CHART 2: DAILY PREPARED VS CONSUMED
  # ======================================
  
  output$daily_plot <- renderPlot({
    
    data <- filtered_data()
    
    validate(
      need(
        nrow(data) > 0,
        "No data matches the selected filters."
      )
    )
    
    daily_summary <- aggregate(
      cbind(Prepared_kg, Consumed_kg) ~ Date,
      data = data,
      FUN = sum
    )
    
    daily_long <- rbind(
      
      data.frame(
        Date = daily_summary$Date,
        Amount = daily_summary$Prepared_kg,
        Type = "Prepared"
      ),
      
      data.frame(
        Date = daily_summary$Date,
        Amount = daily_summary$Consumed_kg,
        Type = "Consumed"
      )
    )
    
    ggplot(
      daily_long,
      aes(
        x = Date,
        y = Amount,
        color = Type,
        group = Type
      )
    ) +
      
      geom_line() +
      
      geom_point(size = 2) +
      
      labs(
        title = "Daily Food Quantities",
        x = "Date",
        y = "Amount (kg)",
        color = "Category"
      ) +
      
      theme_minimal(base_size = 12)
  })
  
  # ======================================
  # DATA TABLE
  # ======================================
  
  output$data_table <- renderDT({
    
    data <- filtered_data()
    
    display_columns <- c(
      "Date",
      "Food_Item",
      "Prepared_kg",
      "Served_kg",
      "Consumed_kg",
      "Unserved_kg",
      "Plate_Waste_kg",
      "Total_Waste_kg",
      "Waste_Percentage"
    )
    
    datatable(
      data[, display_columns, drop = FALSE],
      rownames = FALSE,
      options = list(
        pageLength = 10,
        scrollX = TRUE
      )
    ) |>
      
      formatRound(
        columns = c(
          "Prepared_kg",
          "Served_kg",
          "Consumed_kg",
          "Unserved_kg",
          "Plate_Waste_kg",
          "Total_Waste_kg",
          "Waste_Percentage"
        ),
        digits = 2
      )
  })
  
  # ======================================
  # RECOMMENDATIONS
  # ======================================
  
  output$recommendations <- renderUI({
    
    data <- filtered_data()
    
    if (nrow(data) == 0) {
      return(
        p("No rows match the selected filters.")
      )
    }
    
    waste_summary <- aggregate(
      Total_Waste_kg ~ Food_Item,
      data = data,
      FUN = sum
    )
    
    waste_summary <- waste_summary[
      order(
        waste_summary$Total_Waste_kg,
        decreasing = TRUE
      ),
    ]
    
    highest_waste_item <- waste_summary$Food_Item[1]
    
    highest_waste_amount <- round(
      waste_summary$Total_Waste_kg[1],
      2
    )
    
    total_prepared <- sum(
      data$Prepared_kg,
      na.rm = TRUE
    )
    
    total_waste <- sum(
      data$Total_Waste_kg,
      na.rm = TRUE
    )
    
    waste_percentage <- if (total_prepared > 0) {
      round(total_waste / total_prepared * 100, 2)
    } else {
      0
    }
    
    tagList(
      
      p(
        paste0(
          "Highest-waste item: ",
          highest_waste_item,
          " (",
          highest_waste_amount,
          " kg)."
        )
      ),
      
      p(
        paste0(
          "Waste percentage for the selected data: ",
          waste_percentage,
          "%."
        )
      ),
      
      tags$ul(
        
        tags$li(
          paste(
            "Review portion sizes and preparation quantities for",
            highest_waste_item,
            "."
          )
        ),
        
        tags$li(
          "Use previous consumption records to plan daily preparation."
        ),
        
        tags$li(
          "Track leftovers and adjust quantities for future meals."
        )
      )
    )
  })
  
  # ======================================
  # DOWNLOAD FILTERED DATA
  # ======================================
  
  output$download_data <- downloadHandler(
    
    filename = function() {
      paste0(
        "food_waste_filtered_",
        Sys.Date(),
        ".csv"
      )
    },
    
    content = function(file) {
      write.csv(
        filtered_data(),
        file,
        row.names = FALSE
      )
    }
  )
}

# ==========================================
# 8. RUN SHINY APPLICATION
# ==========================================

shinyApp(
  ui = ui,
  server = server
)
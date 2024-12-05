library(shiny)

library(arrow)

library(jsonlite)

# Load the Parquet file

file_path <- "SMALL_google_news_sentiment.parquet"  # Adjust path as needed

data <- read_parquet(file_path)

# Define colors for entities

icon_fa <- function(.fa_class){
  htmltools::tags$i(class=.fa_class, role="resentation")
}

entity_colors <- c(
  
  LOC = "lightblue", 
  
  PER = "lightgreen", 
  
  ORG = "lightcoral", 
  
  MISC = "lightyellow"
  
)

# Preprocess data to generate highlighted content

preprocess_data <- function(data) {
  
  # Initialize highlighted_content column with the original content
  
  data$highlighted_content <- data$Content
  
  for (i in seq_len(nrow(data))) {
    
    content <- data$Content[i]
    
    entities <- data$entities[[i]]  # Extract the tibble of entities for this row
    
    # Skip rows where content is missing or entities are NULL
    
    if (is.null(content) || is.na(content)) next
    
    if (is.null(entities) || nrow(entities) == 0) {
      
      # No entities for this row, keep original content
      
      data$highlighted_content[i] <- content
      
      next
      
    }
    
    # Highlight entities in the content
    
    for (j in seq_len(nrow(entities))) {
      
      word <- entities$word[j]
      
      entity_group <- entities$entity_group[j]
      
      color <- if (!is.null(entity_colors[[entity_group]])) {
        
        entity_colors[[entity_group]]
        
      } else {
        
        "yellow"
        
      }
      
      # Replace the word in the content with highlighted HTML
      
      content <- gsub(
        
        paste0("\\b", word, "\\b"),  # Match exact word
        
        sprintf("<mark style='background-color: %s;'>%s</mark>", color, word),
        
        content
        
      )
      
    }
    
    # Save the highlighted content back to the column
    
    data$highlighted_content[i] <- content
    
  }
  
  return(data)
  
}

# Preprocess the dataset

data <- preprocess_data(data)

# Shiny UI

ui <- nd.util::nd_page(
  .page_type="app",
  .navbar=NULL,
  .main=list(
    tags$div(
      class="my-4",
      bslib::input_task_button(
        id="generate", class="block bg-primary text-white",
        label="Erhalte einen zufälligen Google News Artikel", 
        icon=icon_fa("fa-solid fa-dice"),
        label_busy="Erhalte einen zufälligen Google News Artikel",
        icon_busy=icon_fa("fa-solid fa-dice fa-spin"),
        style="width: 100%; padding: 8px 16px;"
      ),
      tags$script("$('#generate').click();")
    ),
    uiOutput("article_output"),
    htmltools::suppressDependencies("font-awesome")
  )
)

# Shiny Server

server <- function(input, output, session) {
  
  random_article <- reactiveVal(NULL)
  
  observeEvent(input$generate, {
    
    # Select a random row
    
    random_row <- data[sample(nrow(data), 1), ]
    
    # Create HTML for the article
    
    article_html <- HTML(sprintf("
<h2>%s</h2>
<p><strong>Sentiment:</strong> <span style='color: %s;'>%s</span></p>
<p><strong>Source:</strong> <a href='%s' target='_blank'>%s</a></p>
<p><strong>Keyword:</strong> %s</p>
<p>%s</p>",
                                 
                                 random_row$Title_x,
                                 
                                 if (random_row$Sentiment == "positive") "green" else if (random_row$Sentiment == "neutral") "grey" else "red",
                                 
                                 random_row$Sentiment,
                                 
                                 random_row$Source_Link,
                                 
                                 random_row$Source_Link,
                                 
                                 random_row$keyword,
                                 
                                 random_row$highlighted_content
                                 
    ))
    
    #html Legende für Entitäten
    
    legend_html <- HTML("
<div style='margin-top: 20px;'>
<strong>Legend:</strong>
<span style='background-color: lightblue; padding: 3px 8px; margin: 5px;'>LOC (Location)</span>
<span style='background-color: lightgreen; padding: 3px 8px; margin: 5px;'>PER (Person)</span>
<span style='background-color: lightcoral; padding: 3px 8px; margin: 5px;'>ORG (Organization)</span>
<span style='background-color: lightyellow; padding: 3px 8px; margin: 5px;'>MISC (Miscellaneous)</span>
</div>

    ")
    
    # Combine the article and the legend
    
    combined_html <- HTML(paste(as.character(article_html), as.character(legend_html), sep = "<br/>"))
    
    # Update the reactive value
    
    random_article(combined_html)
    
  })
  
  output$article_output <- renderUI({
    
    random_article()
    
  })
  
}

# Run the Shiny App

shinyApp(ui, server)

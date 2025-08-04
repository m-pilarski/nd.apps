library(shiny)
library(tm)
library(SnowballC)

shiny::addResourcePath("assets", here::here("nd.assets", "assets"))

# example sentences to try out
demo_texts <- c(
  "Uff... der Akku hält keine 3 Stunden mehr nach nur einem Jahr. https://kopfhörer.de/review",
  "Tolles Design, aber der Lautsprecher rauscht ständig https://lautsprecher.de/review",
  "Der Kundendienst hat mir super geholfen – danke!",
  "Kaum ist die Garantie abgelaufen, geht das Gerät kaputt http://techblog.de/review",
  "Warum dauert die Lieferung so lange? 📦 Ich warte nun seit zwei Wochen... https://techblog.de/review"
)

# preprocessing function
preprocess_text <- function(text, steps) {
  original_text <- text

  if ("url" %in% steps) {
    text <- gsub("https?://\\S+|www\\.\\S+", "", text)
  }

  if ("lower" %in% steps) {
    text <- tolower(text)
  }

  if ("punct" %in% steps) {
    text <- gsub("[[:punct:]]", " ", text)
  }

  if ("emoji" %in% steps) {
    text <- gsub("<EMO_[A-Z_]+>", "", text)
  }

  if ("stopwords" %in% steps) {
    text <- removeWords(text, stopwords("de"))
  }

  if ("stem" %in% steps) {
    tokens <- unlist(strsplit(text, "\\s+"))
    tokens <- tokens[tokens != ""]
    text <- paste(wordStem(tokens, language = "german"), collapse = " ")
  }

  if ("token" %in% steps) {
    tokens <- unlist(strsplit(text, "\\s+"))
    tokens <- tokens[tokens != ""]
    text <- paste0("[", paste(tokens, collapse = "], ["), "]")
  }

  text <- gsub("\\s+", " ", text)
  text <- trimws(text)

  return(text)
}



# UI

ui <- nd.util::nd_page(
  .page_type = "app",
  .navbar = NULL,
  .main = list(
    tags$head(
      tags$style(HTML("
        .text-display {
          word-wrap: break-word !important;
          overflow-wrap: break-word !important;
          white-space: pre-wrap !important;
          font-family: 'Courier New', monospace;
          font-size: 16px;
          line-height: 1.4;
          padding: 12px;
          min-height: 100px;
          max-height: 140px;
          overflow-y: auto;
        }
        
        .demo-layout {
          display: flex;
          gap: 1.5rem;
          max-width: 900px;
          margin: auto;
        }
        
        .controls-section {
          flex: 0 0 280px;
        }
        
        .text-sections {
          flex: 1;
          display: flex;
          flex-direction: column;
          gap: 1rem;
        }
        
        @media (max-width: 768px) {
          .demo-layout {
            flex-direction: column;
            gap: 1rem;
          }
          
          .controls-section {
            flex: none;
          }
        }
      "))
    ),
    
    tags$div(
      style = "padding-top: 1rem;",
      
      tags$div(
        class = "demo-layout",
        
        # Left side
        tags$div(
          class = "controls-section",
          tags$div(
            class = "card h-100",
            tags$div(
              class = "card-header", 
              tags$i(class = "fa-solid fa-wrench me-2"), 
              "Schritte auswählen"
            ),
            tags$div(
              class = "card-body",
              checkboxGroupInput(
                inputId = "steps",
                label = NULL,
                choices = c(
                  "Kleinschreibung" = "lower",
                  "Satzzeichen entfernen" = "punct",
                  "URLs entfernen" = "url",
                  "Stoppwörter entfernen" = "stopwords",
                  "Stemming" = "stem",
                  "Tokenisierung" = "token"
                )
              ),
              tags$div(
                class = "mt-3 d-grid gap-2",
                nd.util::nd_button_block("reset", "Zurücksetzen", "fa-solid fa-rotate-left"),
                nd.util::nd_button_block("new_example", "Neues Beispiel", "fa-solid fa-dice")
              )
            )
          )
        ),
        
        # Right side
        tags$div(
          class = "text-sections",
          
          # Original text 
          tags$div(
            class = "card",
            tags$div(
              class = "card-header",
              tags$i(class = "fa-solid fa-file-text me-2"),
              "Originaltext"
            ),
            tags$div(
              class = "card-body p-0",
              tags$div(
                class = "text-display",
                verbatimTextOutput("original_text")
              )
            )
          ),
          
          # Processed text 
          tags$div(
            class = "card",
            tags$div(
              class = "card-header",
              tags$i(class = "fa-solid fa-magic me-2"),
              "Verarbeiteter Text"
            ),
            tags$div(
              class = "card-body p-0",
              tags$div(
                class = "text-display",
                verbatimTextOutput("processed_text")
              )
            )
          )
        )
      )
    )
  )
)


# Server
server <- function(input, output, session) {
  current_text <- reactiveVal(sample(demo_texts, 1))
  
  observeEvent(input$new_example, {
    current_text(sample(demo_texts, 1))
  })
  
  observeEvent(input$reset, {
    updateCheckboxGroupInput(session, "steps", selected = character(0))
  })
  
  output$original_text <- renderText({
    current_text()
  })
  
  output$processed_text <- renderText({
    processed <- preprocess_text(current_text(), input$steps)
    if (nchar(processed) == 0) {
      return("(leer)")
    }
    return(processed)
  })
}


shinyApp(ui, server)
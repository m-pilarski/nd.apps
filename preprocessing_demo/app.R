# app.R
library(shiny)
library(tm)
library(SnowballC)
library(udpipe)
library(here)


### demo texts (could add a few more)
demo_texts <- c(
  "Uff 😠... der Akku hält keine 3 Stunden mehr nach nur einem Jahr. https://kopfhörer.de/review",
  "Tolles Design, aber der Lautsprecher rauscht nach 2 Wochen ständig 😕 https://lautsprecher.de/review",
  "Der Kundendienst hat mir super geholfen – da gibt man gerne 5 Sterne! 😊 https://lautsprecher.de/review",
  "Kaum ist die Garantie abgelaufen, geht das Gerät kaputt 👎 Jetzt warte ich seit über 3 Tagen auf eine Antwort vom Kundendienst http://techblog.de/review",
  "Warum dauert die Lieferung so lange? 📦 Ich warte nun seit 2 Wochen... https://techblog.de/review"
)


### Selection options
STEPS <- c(
  "Kleinschreibung"       = "lower",
  "Satzzeichen entfernen" = "punct",
  "Zahlen entfernen"      = "numbers",
  "URLs entfernen"        = "url",
  "Emojis entfernen"      = "emoji",
  "Stopwörter entfernen"  = "stopwords",
  "Lemmatisierung"        = "lemma",
  "Stemming"              = "stem",
  "Tokenisierung"         = "token"
)


### lemmatization model 
UDPIPE_MODEL <- udpipe_load_model(
   here::here("preprocessing_demo", "german-gsd-ud-2.5-191206.udpipe")
  )

lemmatize_text <- function(text) {
  if (is.null(UDPIPE_MODEL)) return(NULL)   
  ann <- udpipe::udpipe_annotate(UDPIPE_MODEL, x = text)
  df  <- as.data.frame(ann)
  lem <- df$lemma
  lem <- lem[!is.na(lem) & nzchar(lem) & df$upos != "PUNCT"]
  if (!length(lem)) return("")
  paste(lem, collapse = " ")
}


# helpers
sub_fixed <- function(pattern, replacement, x) gsub(pattern, replacement, x, fixed = TRUE)

preprocess_text <- function(text, steps) {
  text <- if (is.null(text)) "" else text
  
  # Detect URLs & emojis once
  url_pattern   <- "(https?://|www\\.)\\S+"
  urls          <- regmatches(text, gregexpr(url_pattern, text, perl = TRUE))[[1]]
  have_urls     <- length(urls) > 0
  
  emoji_pattern <- "[\\p{Extended_Pictographic}\\p{Emoji}\\p{Emoji_Presentation}\\p{Emoji_Component}\\x{FE0F}\\x{200D}]+"
  emojis        <- regmatches(text, gregexpr(emoji_pattern, text, perl = TRUE))[[1]]
  have_emojis   <- length(emojis) > 0
  
  placeholders_url <- character(0)
  if ("punct" %in% steps && !("url" %in% steps) && have_urls) {
    placeholders_url <- sprintf("URLTOKEN%05d", seq_along(urls))  
    tmp <- text
    for (i in seq_along(urls)) tmp <- sub_fixed(urls[i], placeholders_url[i], tmp)
    text <- tmp
  }
  
  placeholders_emo <- character(0)
  if ("punct" %in% steps && !("emoji" %in% steps) && have_emojis) {
    placeholders_emo <- sprintf("EMOTOKEN%05d", seq_along(emojis))
    tmp <- text
    for (i in seq_along(emojis)) tmp <- sub_fixed(emojis[i], placeholders_emo[i], tmp)
    text <- tmp
  }
  
  if ("url"     %in% steps) text <- gsub(url_pattern,   "", text, perl = TRUE)
  if ("lower"   %in% steps) text <- tolower(text)
  if ("emoji"   %in% steps) text <- gsub(emoji_pattern, "", text, perl = TRUE)
  if ("numbers" %in% steps) text <- gsub("[0-9]+",       " ", text)
  if ("punct"   %in% steps) text <- gsub("[[:punct:]]",  " ", text)
  
  if ("lemma" %in% steps) {
    lem <- lemmatize_text(text)
    if (!is.null(lem)) text <- lem
  }
  
  if ("stopwords" %in% steps) text <- tm::removeWords(text, tm::stopwords("de"))
  
  if ("stem" %in% steps) {
    toks <- strsplit(text, "\\s+")[[1]]
    toks <- toks[toks != ""]
    text <- paste(SnowballC::wordStem(toks, language = "german"), collapse = " ")
  }
  
  if (length(placeholders_url) > 0) {
    for (i in seq_along(placeholders_url)) text <- sub_fixed(placeholders_url[i], urls[i], text)
  }
  if (length(placeholders_emo) > 0) {
    for (i in seq_along(placeholders_emo)) text <- sub_fixed(placeholders_emo[i], emojis[i], text)
  }
  
  if ("token" %in% steps) {
    toks <- strsplit(text, "\\s+")[[1]]
    toks <- toks[toks != ""]
    text <- paste0("[", paste(toks, collapse = "], ["), "]")
  }
  
  text <- gsub("\\s+", " ", text)
  trimws(text)
}


# ui
ui <- nd.util::nd_page(
  .page_type = "app",
  .navbar = NULL,
  .main = list(
    tags$head(
      tags$style(HTML("
        .text-display, .text-display pre {
          white-space: pre-wrap !important;
          word-break: break-word !important;
          overflow-wrap: anywhere !important;
          font-family: 'Courier New', monospace;
          font-size: 16px; line-height: 1.45;
          padding: 12px; max-width: 100%; overflow-x: hidden;
        }
        .card { border: none; border-radius: 12px; box-shadow: 0 8px 24px rgba(0,0,0,.06); }
        .card-header { font-weight: 600; }

        /* nicer spacing for controls */
        .selectall-row { 
          padding: .25rem 0 .75rem 0; 
          border-bottom: 1px solid rgba(0,0,0,.06);
          margin-bottom: .5rem;
        }
        .shiny-input-checkboxgroup .shiny-options-group .form-check {
          margin-bottom: .4rem;
        }
      "))
    ),
    
    tags$div(class = "container py-3",
             tags$div(class = "row g-4",
                      
                      # TOP:
                      tags$div(class = "col-12",
                               tags$div(class = "card",
                                        tags$div(class = "card-header",
                                                 tags$i(class = "fa-solid fa-wrench me-2"), "Schritte auswählen"
                                        ),
                                        tags$div(class = "card-body",
                                                 
                                                 tags$div(class = "selectall-row",
                                                          checkboxInput("select_all", "Alles auswählen", value = FALSE)
                                                 ),
                                                 
                                                 checkboxGroupInput(
                                                   inputId = "steps", label = NULL,
                                                   choices = STEPS, inline = FALSE
                                                 ),
                                                 
                                                 tags$div(class = "mt-3 d-grid gap-2",
                                                          nd.util::nd_button_block("new_example", "Neues Beispiel", "fa-solid fa-dice"),
                                                          nd.util::nd_button_block("reset", "Zurücksetzen", "fa-solid fa-rotate-left")
                                                 )
                                        )
                               )
                      ),
                      
                      # BOTTOM: 
                      tags$div(class = "col-12",
                               tags$div(class = "card h-100",
                                        tags$div(class = "card-header",
                                                 tags$i(class = "fa-solid fa-magic me-2"), "Beispieltext (verarbeitet)"
                                        ),
                                        tags$div(class = "card-body p-0",
                                                 tags$div(class = "text-display text-break",
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
    updateCheckboxInput(session, "select_all", value = FALSE)
  })
  
  observeEvent(input$select_all, {
    updateCheckboxGroupInput(
      session, "steps",
      selected = if (isTRUE(input$select_all)) unname(STEPS) else character(0)
    )
  })
  
  observeEvent(input$steps, {
    all_selected <- length(input$steps) == length(STEPS)
    if (isTRUE(input$select_all) != all_selected) {
      updateCheckboxInput(session, "select_all", value = all_selected)
    }
  }, ignoreInit = TRUE)
  
  output$processed_text <- renderText({
    txt <- preprocess_text(current_text(), input$steps)
    if (nchar(txt) == 0) "(leer)" else txt
  })
}

shinyApp(ui, server)
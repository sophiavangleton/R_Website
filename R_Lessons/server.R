#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
library(shiny)

# Load item bank
item_bank <- read.csv("item_bank.csv", stringsAsFactors = FALSE)

# Difficulty-based next item selector
select_next_item <- function(item_bank, used_ids, current_difficulty) {
  remaining <- item_bank[!item_bank$id %in% used_ids, ]
  candidates <- remaining[remaining$difficulty == current_difficulty, ]
  if (nrow(candidates) == 0) candidates <- remaining
  selected <- candidates[sample(nrow(candidates), 1), ]
  return(selected)
}

server <- function(input, output, session) {
  
  rv <- reactiveValues(
    used_ids = c(),
    current_difficulty = 2,
    current_item = NULL,
    iteration = 0,
    done = FALSE,
    feedback = NULL,
    correct_count = 0
  )
  
  # Select first item
  observeEvent(TRUE, {
    rv$current_item <- select_next_item(item_bank, rv$used_ids, rv$current_difficulty)
  }, once = TRUE)
  
  # Display question
  output$question_ui <- renderUI({
    req(!rv$done)
    item <- rv$current_item
    list(
      h3(item$question),
      radioButtons(
        "answer",
        "Choose one:",
        choices = as.character(unlist(item[c("choice1","choice2","choice3","choice4")]))
      ),
      actionButton("submit", "Submit")
    )
  })
  
  # Color-coded feedback
  output$feedback_ui <- renderUI({
    req(rv$feedback)
    
    color <- if (rv$feedback == "Correct!") "green" else "red"
    
    div(
      style = paste0("margin-top: 10px; font-weight: bold; color:", color, ";"),
      rv$feedback
    )
  })
  
  # Handle submission
  observeEvent(input$submit, {
    req(input$answer)
    
    rv$iteration <- rv$iteration + 1
    item <- rv$current_item
    
    # Score based on matching selected text to correct text
    correct <- as.integer(
      input$answer == item[[ paste0("choice", item$answer) ]]
    )
    
    # Store feedback
    rv$feedback <- if (correct == 1) "Correct!" else "Incorrect."
    
    # Track total correct answers
    rv$correct_count <- rv$correct_count + correct
    
    # Update difficulty
    if (correct == 1) {
      rv$current_difficulty <- min(rv$current_difficulty + 1, 3)
    } else {
      rv$current_difficulty <- max(rv$current_difficulty - 1, 1)
    }
    
    # Mark item as used
    rv$used_ids <- c(rv$used_ids, item$id)
    
    # Stopping rule: 10 questions
    if (rv$iteration >= 10) {
      rv$done <- TRUE
    } else {
      rv$current_item <- select_next_item(item_bank, rv$used_ids, rv$current_difficulty)
    }
  })
  
  # Final results summary
  output$results_ui <- renderUI({
    req(rv$done)
    
    score_text <- paste0("You answered ", rv$correct_count, " out of 10 questions correctly.")
    
    div(
      h3("Quiz Complete"),
      h4(score_text),
      p("Thank you for taking this quiz! If you scored below a 80% please revisit prior weeks. This quiz uses adaptive learning to fit your personal skill level.")
    )
  })
}

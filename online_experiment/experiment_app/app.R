library(shiny, quietly = TRUE)
library(DT, quietly = TRUE)
library(shinyjs, quietly = TRUE)
library(lubridate)

check_questions <- read.csv("check_questions.csv", stringsAsFactors = FALSE)
check_answers <- read.csv("check_answers.csv", stringsAsFactors = FALSE)

# Data is retrieved from local repo - need to move data to app directory
# for server deployment
li <- read.csv("../data/online_exp_line_items.csv", stringsAsFactors = FALSE)

disableActionButton <- function(id, session) {
  session$sendCustomMessage(type="jsCode",
                            list(code= paste("$('#", id, "').prop('disabled',true)"
                                             , sep="")))
}

ui <- fluidPage(
  useShinyjs(),
  uiOutput('greeting'),
  titlePanel("Calculating Adjusted Earnings"),
  br(),
  uiOutput('screen')
)


server <- function(input, output, session) {
  time_in <- Sys.time()
  time_read <- NA
  time_check <- NA
  time_ready <- NA
  time_assignment <- NA
  check_response <- rep(FALSE, 6)

  submitted <- reactiveVal(FALSE)
  intro_screen <- reactiveVal(TRUE)
  check_screen <- reactiveVal(FALSE)
  assignment_screen <- reactiveVal(FALSE)
  survey_closed <- reactiveVal(FALSE)
  has_participated <- reactiveVal(FALSE)
  checked <- reactiveVal(FALSE)
  selectable <- reactiveVal('multiple')
  bground_color <- reactiveVal(rep("white", 4))
  perfect <- reactiveVal(FALSE)
  treatment <- reactiveVal(NA)
  gov_tment <- reactiveVal("")
  emi_tment <- reactiveVal("")
  id <- reactiveVal(NA)
  mchecks_correct <- reactiveVal(rep(NA, 4))
  mchecks_response <- reactiveVal(rep(FALSE, 4))
  mchecks_feedback <- reactiveVal(rep(0, 4))
  
  wave <- isolate(basename(session$clientData$url_pathname))
  qorder <- sample(1:nrow(check_questions))

  treatment(sample(1:4, 1))
  
  observe({
    if (!is.na(treatment())) {
      if (treatment() > 2) {
        gov_tment(HTML("The firm’s <b>corporate governance is assessed by external advisors to be weak</b> compared to its industry peers"))
      } else {
        gov_tment(HTML("The firm’s <b>corporate governance is assessed by external advisors to be strong</b> compared to its industry peers"))
      }
      
      if (treatment() %in% c(2,4)) {
        emi_tment(HTML("The firm <b>bases managerial compensation on net income</b>"))
      } else {
        emi_tment(HTML("The firm <b>bases managerial compensation on internal production targets</b>"))
      }
      mchecks_correct(c(treatment() > 2, treatment() < 3, 
                        treatment() %in% c(2,4), treatment() %in% c(1,3)))
    }
  })
  
  store_user_response <- function(response = NA) {
    rdf <- data.frame(id = id(),
                      tstamp = as.character(Sys.time()),
                      treatment = treatment(),
                      time_read = time_read,
                      time_check = time_check,
                      time_ready = time_ready,
                      time_assignment = time_assignment,
                      stringsAsFactors = FALSE)
    
    if (!is.na(time_check)) {
      rdf$man_check_cgo_correct <- all(mchecks_correct()[1:2] == mchecks_response()[1:2]) 
      rdf$man_check_emi_correct <- all(mchecks_correct()[3:4] == mchecks_response()[3:4]) 
    }
    if (any(!is.na(response))) {
      included <- rep(FALSE, length(li$var[li$fixed == 0]))
      names(included) <- li$var[li$fixed == 0]
      included[response] <- TRUE
      rdf <- data.frame(c(rdf, included), stringsAsFactors = FALSE)
    }  
    
    # Below you need to implement a function that permanently stores
    # the response and updates the response data whenever the recipient
    # has completed the next step. We used a server-side SQLite database 
    # for this purpose.
    #
    # In the experimental demo app no response data are stored.
    
    # update_repsonse(rdf)
  } 

  
  create_ae <- reactive({
    if (length(input$li_rows_selected) > 0) {
      ae_line_items <- c(which(li$fixed == 1),
                         which(li$fixed == 0)[input$li_rows_selected[order(input$li_rows_selected)]])
      ae <- li[ae_line_items, c("line_item", "value")]
      ae <- rbind(ae, list("Your adjusted earnings measure", sum(ae$value)))
    }
    else {
      ae <- li[which(li$fixed == 1),  c("line_item", "value")]
      ae <- rbind(ae, list("Your adjusted earnings measure", sum(ae$value)))
    }
    return(ae)
  })

  
  observeEvent(input$understood, {
    intro_screen(FALSE)
    time_read <<- as.numeric(difftime(Sys.time(), time_in, units = "sec"))
    store_user_response()
    check_screen(TRUE)
  })
  
  observeEvent(input$check, {
    check_screen(TRUE)
    selectable('none')
    time_check <<- as.numeric(difftime(Sys.time(), time_in, units = "sec")) - time_read
  
    cr <- rep(FALSE, 4)
    sr <- rep(FALSE, 4)
    for (i in 1:2) {
      cr[(i - 1)*2 + input[[paste0("check_answer", i, "_rows_selected")]]] <- TRUE
      sr[(2*qorder[i] - 1):(2*qorder[i])] <- cr[(2*i - 1):(2*i)]
    }
    mchecks_response(sr)
    perfect(identical(mchecks_response(), mchecks_correct()))
    mchecks_feedback(2 * as.integer(mchecks_correct()) + as.integer(mchecks_response()))
    bground_color(c("white", "pink", "palegreen", "palegreen"))
    store_user_response()
  })
  
  observeEvent(input$assignment, {
    check_screen(FALSE)
    time_ready <<- as.numeric(difftime(Sys.time(), time_in, units = "sec")) - time_read - time_check
    store_user_response()
    assignment_screen(TRUE)
  })

  observeEvent(input$submit, {
    if (length(input$li_rows_selected) == 0)
      alert("Please select at least one line item!")
    else {
      if (!submitted()) {
        time_assignment <<-as.numeric(difftime(Sys.time(), time_in, units = "sec")) - time_read - time_check - time_ready
        store_user_response(input$li_rows_selected)
        disable("submit")
        submitted(TRUE)
        assignment_screen(FALSE)
       }
    }
  })
  
  output$greeting <- renderUI(
    if (intro_screen()) tagList(
      fluidRow(
        column(12, br(),
               p("Thank you for participating in and supporting our research.",
                 "Our aim is to understand how professional investors use", 
                 "income statement information when assessing firm performance."), 
               p(HTML("Your task consists of a small assignment and",
                      "<b>should not take more than five minutes</b>",
                      "to complete. We designed the task to be as", 
                      "streamlined as possible to minimise the use", 
                      "of your time.")),
               p("Please read the short text below carefully. It contains",
                 "important information for your assignment."),
               br()
               )
      )
    )
  )
  
  output$screen <- renderUI({
    if (intro_screen()) tagList(
      fluidRow(
        column(10, offset = 1, p(HTML("Imagine that you are analyzing a ",
                                      "<b>privately-held European firm</b>", 
                                      "operating in the <b>manufacturing industry</b>.",
                                      paste0(gov_tment(), "."), 
                                      paste0(emi_tment(), "."),
                                      paste("Your task is to calculate adjusted earnings", 
                                            "to assess firm performance."))))
      ),
      fluidRow(
        column(12, align = "center",
               br(),
               p("When you have carefully read the text above, click below to start the assignment."),
               actionButton("understood", "I have read the text. Take me to the assignment", class = "btn-primary")
        )
      )
    ) else if (check_screen()) tagList(
      fluidRow(
        column(12, p("We know you have read the text carefully. But let's make sure nevertheless."),
               uiOutput('check')
        )
      ),
      fluidRow(
        column(12, align = "center",
               br(),
               uiOutput('evaluate_check')
        )
      )
    )
    else if (assignment_screen()) tagList(
      fluidRow(
        column(12, p("Your task is to calculate adjusted earnings",
                     "to assess firm performance.",
                     "Click on the additional line items in the list below",
                     "to include them in your adjusted earnings measure."),
               br(),
               p("Remember:"),
               tags$ul(
                 list(
                   tags$li(HTML("The firm is a <b>privately-held company</b> operating in the <b>manufacturing industry</b>")),  
                   tags$li(gov_tment()),
                   tags$li(emi_tment())
                 )  
               ))
      ),
      
      fluidRow(
        column(6, dataTableOutput('li')),
        column(6, dataTableOutput('ae'))
      ),
      
      fluidRow(
        column(12, align = "center",
               br(),
               p("When you feel that your adjusted earnings measure",
                 "is suitable to assess firm performance",
                 "press the submit button below."),
               actionButton("submit", "Submit", class = "btn-primary"),
               br()
        )
      )
    )
    else if (survey_closed()) fluidRow(
      column(12, align = "center",
             br(),
             p(strong("Thank you for your interest in participating and supporting our research!")),
             p("The web-based task is now closed."),
             br(), 
             p("If you are interested in the outcome of our research,",
               HTML("<a href=\"mailto:someoune@somewhere.com?subject=Your research on adjusted earnings\">send us a messsage!</a>")
             )
      )
    )
    else fluidRow(
      column(12, align = "center",
             br(),
             p(strong("Thank You! Your answer has been recorded.")),
             br(), 
             p("If you are interested in the outcome of our research,",
               HTML("<a href=\"mailto:blinded_authors@binded_uni.edu?subject=Your research on adjusted earnings\">send us a messsage!</a>"),
               "If you have any concerns related to your participation in this", 
               "survey, please contact the",
               HTML("<a href=\"mailto:blinded_committee@blinded_uni.edu\">",
                    "Blinded University Ethics Committee</a>.")
             ),
             br(), 
             p("You can now close this window.")
      )
    )
  })
  
  # JG 2020-10-19: The above contains a typo ("messsage"). We are leaving the 
  #   typo in to keep the experimental demo in sync with the original material.
  
  output$check <- renderUI({
    tl <- tagList(
      p("Please complete the sentences below by clicking on the correct endings."),
      br()
    )
    for (i in 1:2) {
      tl <- list(tl, dataTableOutput(paste0('check_answer', i)), br())
    }
    tl
  })
  
  check_answer_table <- function(j) {
    at <- cbind(check_answers[check_answers$qid == qorder[j], 2], 
                mchecks_feedback()[(1 + (qorder[j] - 1)*2):(2 + (qorder[j] - 1)*2)])
    cname <- 1
    names(cname) <- check_questions$qtext[qorder[j]]
    datatable(as.data.frame(at), 
                    options = list(searching = FALSE,
                                   paging = FALSE,
                                   info = FALSE,
                                   ordering = FALSE,
                                   columnDefs = list(list(visible=FALSE, targets=1))),
                    rownames = FALSE,
                    colnames = cname,
                    selection = 'single'
    ) %>% 
      formatStyle(columns = 1, valueColumns = 2, 
                  backgroundColor = styleEqual(c(0, 1, 2, 3),  bground_color()))
  }
  
  for (i in 1:2) {
    output[[paste0("check_answer", i)]] <- renderDataTable(server = FALSE, 
                                                           expr = bquote(check_answer_table(.(i))),
                                                           quoted = TRUE)
  }
  
  output$evaluate_check <- renderUI({
    req(input$understood)
    if(input$understood & !(selectable() == 'none')) {
      return(tagList(p("If you are sure you got this right,",
                       "then press the check button below."),
                     actionButton("check", "Check", class = "btn-primary")))
    } else {
      if (perfect()) tl <- p("Perfect! Time to move to the assignment. Click below to continue")
      else tl <- p("Not quite! Please take a look at the correct statements marked in green. Then click below to continue")
      return(tagList(tl,
                     actionButton("assignment", "Continue", class = "btn-primary")))
    }
  })
  
  output$li <- renderDataTable(
    server = FALSE,
    {
      datatable(li[which(li$fixed == 0), c("line_item", "value")], options = list(searching = FALSE,
                                 paging = FALSE,
                                 info = FALSE,
                                 ordering = FALSE),
                rownames = FALSE,
                colnames = c("Additional income statement line items" = 1,
                             "Value in €m" = 2)
                ) %>% formatCurrency(2, currency ="", interval=3, 
                                     mark=',', digits=0)
    }
  )

  output$ae <- renderDataTable(
    server = FALSE,
    {
      ae <- create_ae()
      datatable(ae, options = list(searching = FALSE,
                                 paging = FALSE,
                                 info = FALSE,
                                 ordering = FALSE),
                selection = 'none',
                rownames = FALSE,
                colnames = c("Adjusted earnings calculation" = 1,
                             "Value in €m" = 2)
      ) %>% 
        formatCurrency(2, currency ="", interval=3, mark=',', digits=0) %>%
        formatStyle(1, target = "row", 
                    fontWeight = styleEqual("Your adjusted earnings measure", "bold"))
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)


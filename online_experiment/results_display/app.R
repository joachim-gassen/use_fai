library(DBI, quietly = TRUE)
library(shiny, quietly = TRUE)
library(DT, quietly = TRUE)
library(shinyjs, quietly = TRUE)
library(tidyverse, quietly = TRUE)
library(kableExtra, quietly = TRUE)
library(lubridate, quietly = TRUE)
library(lfe, quietly = TRUE)
library(ExPanDaR, quietly = TRUE)
library(scales, quietly = TRUE)

# Data is retrieved from local repo - need to move data to app directory
# for server deployment
li <- read.csv("../data/online_exp_line_items.csv", stringsAsFactors = FALSE)

plot_resp_over_time <- function(df) {
  df <- df %>%
    filter(!is.na(time_read)) %>%
    mutate(tstamp = ymd_hms(tstamp))
  
  ggplot(df, aes(x = tstamp)) + 
    geom_histogram(binwidth=3600) +
    scale_x_datetime(
      name = "Date and time [UTC]",
      date_labels = "%b %d", 
      date_breaks = "1 day",
      date_minor_breaks = "6 hours",
      limits = c(ymd_hms("2019-06-26 00:00:00"), 
                 ymd_hms("2019-07-16 11:49:08")),
      expand = c(0,0)) +
    ylab("Responses per hour") +
    theme_minimal()
}


tab_vars <- c(
  li$line_item[3:9], 
  "(A) Weak Corporate Governance", "(B) Earnings Management Incentives", 
  "(C) Managerial Discretion Gain Line Item * (A)", 
  "(D) Managerial Discretion Gain Line Item * (B)", 
  "(A) * (B)", "(C) * (D)"
)
names(tab_vars) <- c(
  "line_itemsga", "line_itemda", "line_itemfvg_market",
  "line_itemfvg_model", "line_itemgains_inv_property",
  "line_itemint_exp", "line_itemtax_exp",
  "treated_cg", "treated_em", "md_tcg", "md_tem", 
  "treated_cgTRUE:treated_em", "md_tcgTRUE:md_tem"
)

change_var_names <- function(html_tab) {
  # Make sure that the longer patterns are listed first
  var_names <- c(
    "line_itemsga", "line_itemda", "line_itemfvg_market",
    "line_itemfvg_model", "line_itemgains_inv_property",
    "line_itemint_exp", "line_itemtax_exp",
    "dinclude", "fvg_model",	"gains_inv_property",	
    "treated_cgTRUE:treated_em", "md_tcgTRUE:md_tem",
    "treated_cg", "treated_em", "md_tcg", "md_tem", 
    "included", "id", "line_item"
  )
  tab_vars <- c(
    li$line_item[3:9], 
    "\u0394Include", paste("Include", li$line_item[6:7]),
    "(A) * (B)", "(C) * (D)",
    "(A) Weak Corporate Governance", "(B) Earnings Management Incentives", 
    "(C) Managerial Discretion Gain Line Item * (A)", 
    "(D) Managerial Discretion Gain Line Item * (B)", 
    "Line Item Included", "Respondent", "Line Item"
  )
  pre <- '\\b'
  post <- '\\b'
  
  patterns <- paste0(pre, var_names, post)
  pattern <- paste0("(",paste(patterns, collapse = "|"),")")
  coef_rows <- which(str_count(html_tab, pattern) == 1)
  
  for (i in coef_rows) {
    html_tab[i] <- str_replace(
      html_tab[i], 
      paste0("<td[^>]*>(?=", pattern, ")"),
      "<td style=\"text-align:left; vertical-align:top\" rowspan=2>"
    )
    html_tab[i + 1] <- sub("<td[^>]*><\\/td>", "", html_tab[i + 1])
  }

  replacements <- tab_vars
  names(replacements) <- paste0(pre, var_names, post)
  str_replace_all(html_tab, replacements)
}


ui <- fluidPage(
  tags$head(
    tags$style(HTML(
      "
      #subject_level_regressions table > thead > tr > td,
      #subject_level_regressions table > tbody > tr > td,
      #subject_level_regressions table > tfoot > tr > td {
      padding:0px 5px;
      }"))
  ),
  titlePanel("Cascino et al. (2020): Online Experiment Evaluation"),
  fluidRow(
    column (
      12,
      p("This interactive display allows you to download the data and explore",
        "the findings of the online experiment presented in the study",
        HTML("<a href=https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3008083>",
          "'The Usefulness of Financial Accounting Information: Evidence from the",
          "Field'</a>"),
          "by Stefano Cascino, Mark Clatworthy, Beatriz García Osma,",
          "Joachim Gassen and Shahed Imam."          
      ),
      p("In the column to the left below the response data, you can modify",
        "the sample definition. In the main tab, you find various visuals and", 
        "regression analyses for your selected sample."),
      p("In addition, you can download the response data for more in depth",
        "analysis.")
    ),
    hr()
  ),
  fluidRow(
    column(
      12,
      h3("Email Administration"),
      plotOutput("response_over_time"),
      tableOutput("email_delivery")
    )
  ),
  hr(),
  fluidRow(
    column(
      4,
      selectInput("location", "Select locations to include",
                  c("All", 
                    "Europe", 
                    "North America",
                    "Rest of World"),
                  selected = "All", multiple = TRUE
      ),
      radioButtons("occupation", "Limit the sample to certain occupations?",
                  c("All", 
                    "Financial Analyst", 
                    "Portfolio Manager"),
                  selected = "All"
      ),
      radioButtons("data_cutoff", "Limit the sample to subjects that passed manipulation checks?",
                   c("All observations" = "none",
                     "Only observations with passed manipulation checks" = "mchecks_passed"),
                   selected = "mchecks_passed"),
      br(),
      sliderInput("exclude_below_time",
                  "Only observations with assignment time in seconds larger than...",
                  value = 0,
                  min = 0,
                  max = 120),
      downloadButton("download", "Download the experimental data")
    ),
    column(
      8,
      h3("Experimental Sample by Treatment"),
      tableOutput("experimental_sample")
    )
  ),
  hr(),
  fluidRow(
    column(
      4,
      h4("Line item selection behavior overall")
    ),
    column(
      8,
      plotOutput("line_items_selected")
    )
  ),
  hr(),
  fluidRow(
    column(
      4,
      h4("Treatment effects for gain line items")
    ),
    column(
      8,
      plotOutput("gain_line_items_by_tment"),
      plotOutput("dprob_disc_non_disc"),
      plotOutput("tment_effect_disc_rel_to_non_disc")
    )
  ),
  hr(),
  fluidRow(
    column(
      4,
      h4("Subject-level test results")
    ),
    column( 
      8, align="center",
      htmlOutput("subject_level_regressions")
    )
  ),
  hr(),
  fluidRow(
    column(
      4,
      h4("Line-item test results - All line items")
    ),
    column( 
      8, align="center",
      htmlOutput("all_line_items_regressions")
    )
  ),
  hr(),
  fluidRow(
    column(
      4,
      h4("Line-item test results - Only gain line items")
    ),
    column( 
      8, align="center",
      htmlOutput("gain_line_items_regressions")
    )
  ),
  hr(),
  fluidRow(
    column(
      4,
      h4("Line-item test results - All line items with fixed effects")
    ),
    column( 
      8, align="center",
      htmlOutput("all_line_items_fe_regressions")
    )
  ),
  hr(),
  fluidRow(
    column(
      4,
      h4("Line-item test results - Only gain line items with fixed effects")
    ),
    column( 
      8, align="center",
      htmlOutput("gain_line_items_fe_regressions")
    )
  ),
  hr()
)


server <- function(input, output, session) {
  # Data is retrieved from local repo - need to move data to app directory
  # for server deployment
  resp <- read_csv("../data/online_exp_response_data.csv", col_types = cols())
  emails <- read_csv("../data/online_exp_emails_sent.csv", col_types = cols())
  
  sent_wave <- as.data.frame(table(emails$wave), stringsAsFactors = FALSE)
  colnames(sent_wave) <- c("wave", "emails_sent")
  sent_wave <- 
    cbind(sent_wave, 
          occupation = c(rep("Portfolio Manager", 3),
                         rep("Financial Analyst", 3)), 
          location = rep(c("Rest of World", "Europe", "North America"), 2))
  
  resp %>%
    filter(!is.na(time_read)) %>%
    mutate(did_mc = !is.na(time_check),
           passed_both_mc = man_check_cgo_correct & 
             man_check_emi_correct,
           started_assignment = !is.na(time_ready),
           completed_assignment = !is.na(time_assignment),
           dinclude = 0.5*fvg_model + 0.5*gains_inv_property - fvg_market,
           treated_cg = treatment > 2,
           treated_em = treatment %in% c(2,4)) %>%
    left_join(sent_wave, by = c("occupation", "location")) %>%
    select(id, occupation, location, emails_sent, tstamp, 
           treatment, treated_cg, treated_em, 
           did_mc, started_assignment, completed_assignment,
           time_read, time_check, time_ready, time_assignment,
           man_check_cgo_correct, man_check_emi_correct, passed_both_mc,
           one_of(li$var[li$fixed == FALSE]),
           dinclude) -> responded
  
  df_wide <- reactive({
    df <- responded
    if (input$occupation != "All")
      df <- df %>% filter(occupation == input$occupation)
    if (length(input$location) > 0 &&  ! "All" %in% input$location)
      df <- df %>% filter(location %in% input$location)
    if (input$data_cutoff == "mchecks_passed") 
      df <- df %>% filter(man_check_cgo_correct, man_check_emi_correct)
    df %>% filter(time_assignment >= input$exclude_below_time) 
  })
  
  df_long <- reactive({
    df_wide() %>%
      select(id, treatment, one_of(li$var[li$fixed == FALSE])) %>%
      gather(key = "line_item", value = "included", 
                         -id, -treatment) %>%
      mutate(id = factor(id),
             line_item = factor(line_item, levels = li$var[!li$fixed]),
             treated_cg = treatment > 2,
             treated_em = treatment %in% c(2,4),
             man_desc = line_item == "fvg_model" | line_item == "gains_inv_property",
             md_tcg = man_desc & treated_cg,
             md_tem = man_desc & treated_em)
  })
  
  df_long_gain_line_items <- reactive({
    df_long() %>%     
      filter(line_item != "sga",
             line_item != "da",
             line_item != "int_exp",
             line_item != "tax_exp") 
  })
  
  
  output$response_over_time <- renderPlot({
    plot_resp_over_time(resp)
  })

  output$email_delivery <- function() {
    responded %>%
      summarize(
        group = "Total",
        emails_sent = sum(sent_wave$emails_sent),
        responded = n(),
        response_rate = n()/sum(sent_wave$emails_sent),
        received_tment = n(),
        did_mc = sum(did_mc)/n(), 
        passed_both_mc = sum(passed_both_mc, na.rm = TRUE)/n(), 
        started_assignment = sum(started_assignment)/n(),
        completed_assignment = sum(completed_assignment)/n(),
        .groups = "drop"
      ) -> sample_total
    
    responded %>%
      group_by(occupation, location) %>%
      summarize(
        emails_sent = as.integer(min(emails_sent)),
        responded = n(),
        response_rate = n()/min(emails_sent),
        did_mc = sum(did_mc)/n(), 
        passed_both_mc = sum(passed_both_mc, na.rm = TRUE)/n(), 
        started_assignment = sum(started_assignment)/n(),
        completed_assignment = sum(completed_assignment)/n(),
        .groups = "drop"
      ) %>%
      mutate(group = sprintf("%s, %s", occupation, location)) %>%
      select(-occupation, -location) %>%
      select(c(8, 1:7)) %>%
      rbind(sample_total %>% select(-received_tment)) %>%
      mutate_if(is.integer, comma) %>%
      mutate_if(is.double, percent, accuracy = 0.1) -> resp_table
    
    
    resp_table %>%
      kable(col.names = c("Group", 
                          "# emails sent", 
                          "# responded", 
                          "Response rate",
                          "Did manipulation checks",
                          "Passed both manipulation checks",
                          "Started assignment",
                          "Completed assignment"),
            align="lrrrrrrr") %>%
      kable_styling() %>%
      row_spec(7, bold = T) 
  }
  
  output$download <- downloadHandler(
    filename <- function() {
      paste0('online_exp_response_data.csv')
    },
    content <- function(con) {
      write_csv(responded %>% select(-emails_sent), con)
    }
  )
  
  output$experimental_sample <- function() {
    df_wide() %>%
      summarize(
        group = "Total",
        received_tment = n(),
        passed_cgo_mc = sum(man_check_cgo_correct)/n(),
        passed_emi_mc = sum(man_check_emi_correct)/n(),
        passed_both_mc = sum(passed_both_mc)/n(),
        .groups = "drop"
      ) -> sample_total
    
    df_wide() %>%
      group_by(treatment) %>%
      summarize(
        received_tment = n(),
        passed_cgo_mc = sum(man_check_cgo_correct)/n(),
        passed_emi_mc = sum(man_check_emi_correct)/n(),
        passed_both_mc = sum(passed_both_mc)/n(),
        .groups = "drop"
      ) %>%
      rbind(sample_total %>%
              rename(treatment = group)) %>%
      mutate(treatment = case_when(
        treatment == "1" ~ "Strong CG\nNo EMI", 
        treatment == "2" ~ "Strong CG\nEMI",
        treatment == "3" ~ "Weak CG\nNo EMI", 
        treatment == "4" ~ "Weak CG\nEMI",
        TRUE ~ "Total"
      )) %>%
      mutate_if(is.integer, comma) %>%
      mutate_if(is.double, percent, accuracy = 0.1) -> tment_table
    
    tment_table %>%
      kable(col.names = c("Treatment", 
                          "# treated",
                          "Passed CG manipulation check",
                          "Passed EM manipulation check",
                          "Passed both manipulation checks"),
            align="lrrrr") %>%
      kable_styling() %>%
      row_spec(5, bold = T) 
  }

  output$line_items_selected <- renderPlot({
    df <- df_long()
    df$line_item <- factor(df$line_item, 
                           labels = str_wrap(li$line_item[!li$fixed], 10))
    
    ggplot(df, aes(x = line_item, y = as.numeric(included))) +
      stat_summary(fun = mean, geom = "bar", color = "lightblue", fill = "lightblue") + 
      stat_summary(fun.data = mean_se, geom = "errorbar") +
      scale_y_continuous(name = "Percentage of respondents that include",
                         labels = scales::percent_format(accuracy = 1)) +
      scale_x_discrete(name ="Line item") + 
      theme_minimal() 
  })
  
  output$gain_line_items_by_tment <- renderPlot({
    df_long() %>%
      filter(line_item != "sga",
             line_item != "da",
             line_item != "int_exp",
             line_item != "tax_exp") %>%
      mutate(line_item = factor(line_item, levels = li$var[!li$fixed],
                                labels = paste0(str_wrap(li$line_item[!li$fixed], 10), "\n")))-> df
    
    ggplot(df, aes(x = as.factor(treatment), 
                   y = as.numeric(included), 
                   color = line_item)) +
      stat_summary(fun = mean, geom = "point", 
                   size = 2, position = position_dodge(0.2)) + 
      stat_summary(fun.data = mean_se, geom = "errorbar", 
                   width = 0.2, position = position_dodge(0.2)) +
      theme_minimal() + 
      scale_x_discrete(name = "Treatment", 
                       labels = c("Strong CG\nNo EMI", "Strong CG\nEMI",
                                  "Weak CG\nNo EMI", "Weak CG\nEMI")) +
      scale_y_continuous(name = "Probability of inclusion",
                         labels = scales::percent_format(accuracy = 0.5)) +
      scale_color_discrete(name = "Line item")    
  })

  output$dprob_disc_non_disc <- renderPlot({
    ggplot(df_wide(), aes(x = as.factor(treatment), y = dinclude)) +
      stat_summary(fun = mean, geom = "point", 
                   size = 2) + 
      stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
      theme_minimal() + 
      scale_x_discrete(name = "Treatment", 
                       labels = c("Strong CG\nNo EMI", "Strong CG\nEMI",
                                  "Weak CG\nNo EMI", "Weak CG\nEMI")) +
      scale_y_continuous(name = paste("Percentage point difference in inclusion",
                                      "probability for gain line items with", 
                                      "and without managerial discretion", sep = "\n"),
                         labels = unit_format(scale = 100, unit = "pp", accuracy = 0.5))
    
  })

  
  output$tment_effect_disc_rel_to_non_disc <- renderPlot({
    df <- rbind(cbind(manipulation = "cg", df_wide() %>%
                        select(id, treated_cg, treated_em, dinclude)),
                cbind(manipulation = "em", df_wide() %>%
                        select(id, treated_cg, treated_em, dinclude)))
    
    ggplot(df, aes(x = treated_em, y = dinclude, 
                   linetype = treated_cg,
                   group = treated_cg)) +
      stat_summary(fun = mean, geom = "point", 
                   size = 2, position = position_dodge(0.1)) + 
      stat_summary(fun.data = mean_se, geom = "errorbar", 
                   position = position_dodge(0.1), width = 0.1, size = 0.25) +
      stat_summary(fun = mean,
                   geom = "line", position = position_dodge(0.1)) +
      scale_x_discrete(name = "Earnings Management Incentives?", 
                       expand = c(0.05, 0.05), labels = c("No", "Yes")) +
      scale_y_continuous(
        name = paste("Percentage point difference in inclusion",
                     "probability for gain line items with", 
                     "and without managerial discretion", sep = "\n"),
        labels = unit_format(scale = 100, unit = "pp", accuracy = 0.5)
      ) + 
      scale_linetype_discrete(name = "CG Treamtment", 
                              labels = c("Strong", "Weak")) +
      theme_minimal()
  })

  output$subject_level_regressions <- renderPrint({
    tab <- prepare_regression_table(
      df_wide(),
      dvs = c(
        "dinclude", "dinclude", "fvg_model", "fvg_model", 
        "gains_inv_property", "gains_inv_property"
      ), 
      idvs = list(
        c("treated_cg", "treated_em"),
        "treated_cg*treated_em", 
        c("treated_cg", "treated_em"),
        "treated_cg*treated_em",
        c("treated_cg", "treated_em"),
        "treated_cg*treated_em"
      ),
      model = rep("ols", 6)
    )
    htmltools::HTML(change_var_names(tab$table))
  })
  
  output$all_line_items_regressions <- renderPrint({
    tab <- prepare_regression_table(
      df_long(),
      dvs = rep("included", 2), 
      idvs = list(
        c("0", "line_item", "treated_cg", "treated_em", "md_tcg", "md_tem"),
        c("0", "line_item", "treated_cg*treated_em", "md_tcg*md_tem")
      ),
      models = c(rep("ols", 2)),
      cluster = c(rep("id", 2))
    )
    htmltools::HTML(change_var_names(tab$table))
  })
  
  output$gain_line_items_regressions <- renderPrint({
    tab <- prepare_regression_table(
      df_long_gain_line_items(),
      dvs = rep("included", 2), 
      idvs = list(
        c("0", "line_item", "treated_cg", "treated_em", "md_tcg", "md_tem"),
        c("0", "line_item", "treated_cg*treated_em", "md_tcg*md_tem")
      ),
      models = c(rep("ols", 2)),
      cluster = c(rep("id", 2)))
    htmltools::HTML(change_var_names(tab$table))
  })
  
  output$all_line_items_fe_regressions <- renderPrint({
    tab <- prepare_regression_table(
      df_long(), 
      dvs = c(rep("included", 3)), 
      idvs = list(c("md_tcg"), c("md_tem"), c("md_tcg*md_tem")),
      models = c(rep("ols", 3)),
      feffects = rep(list(c("id", "line_item")), 3),
      cluster = rep(list(c("id", "line_item")), 3),
      format = "html"
    )
    htmltools::HTML(change_var_names(tab$table))
  })
  
  output$gain_line_items_fe_regressions <- renderPrint({
    tab <- prepare_regression_table(
      df_long_gain_line_items(), 
      dvs = c(rep("included", 3)), 
      idvs = list(c("md_tcg"), c("md_tem"), c("md_tcg*md_tem")),
      models = c(rep("ols", 3)),
      feffects = rep(list(c("id", "line_item")), 3),
      cluster = rep(list(c("id", "line_item")), 3),
      format = "html"
    )
    htmltools::HTML(change_var_names(tab$table))
  })
}


# Run the application 
shinyApp(ui = ui, server = server)


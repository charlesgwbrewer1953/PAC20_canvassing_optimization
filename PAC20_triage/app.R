library(shiny)
library(DT)
library(plotly)
# Versio 0.0.1
# UI
ui <- fluidPage(
  titlePanel("Political Canvassing Resource Optimization Tool"),
  sidebarLayout(
    sidebarPanel(
      h3("Input Parameters"),
      
      # Basic parameters
      numericInput("total_visits", "Total Visits Available:",
                   value = 400, min = 1, step = 1),
      numericInput("voters_per_oa", "Voters per Output Area:",
                   value = 300, min = 1, step = 1),
      numericInput("response_rate", "Canvassing Response Rate (%):",
                   value = 20, min = 0.1, max = 100, step = 0.1),
      
      hr(),
      h4("Tier 1 (Highest Priority)"),
      numericInput("tier1_oas", "Number of OAs to visit in Tier 1:",
                   value = 107, min = 0, step = 1),
      numericInput("tier1_likelihood", "Reform Support Likelihood (%):",
                   value = 45, min = 0, max = 100, step = 0.1),
      
      h4("Tier 2 (Medium Priority)"),
      numericInput("tier2_oas", "Number of OAs to visit in Tier 2:",
                   value = 107, min = 0, step = 1),
      numericInput("tier2_likelihood", "Reform Support Likelihood (%):",
                   value = 33, min = 0, max = 100, step = 0.1),
      
      h4("Tier 3 (Lowest Priority)"),
      checkboxInput("lock_tier3", "Lock Tier 3 OAs as remainder so Tier1+Tier2+Tier3 = Total Visits",
                    value = TRUE),
      numericInput("tier3_oas", "Number of OAs to visit in Tier 3:",
                   value = 106, min = 0, step = 1),
      numericInput("tier3_likelihood", "Reform Support Likelihood (%):",
                   value = 15, min = 0, max = 100, step = 0.1),
      
      hr(),
      actionButton("optimize", "Optimize Allocation", class = "btn-primary")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Optimization Results",
                 br(),
                 h3("Optimal Visit Allocation"),
                 DT::dataTableOutput("results_table"),
                 br(),
                 h3("Summary Statistics"),
                 verbatimTextOutput("summary_stats"),
                 br(),
                 h3("Resource Allocation Visualization"),
                 plotlyOutput("allocation_plot")
        ),
        
        tabPanel("Sensitivity Analysis",
                 br(),
                 h3("Returns per Visit by Tier"),
                 plotlyOutput("returns_plot"),
                 br(),
                 h3("Marginal Returns Analysis"),
                 plotlyOutput("marginal_plot")
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # --- Auto-calc Tier 3 OAs as remainder to hit total_visits exactly ---
  observeEvent({
    list(input$total_visits, input$tier1_oas, input$tier2_oas, input$lock_tier3)
  }, ignoreInit = TRUE, {
    if (isTRUE(input$lock_tier3)) {
      remainder <- max(0, input$total_visits - input$tier1_oas - input$tier2_oas)
      # Avoid infinite loop by only updating when value differs
      if (!isTRUE(identical(remainder, input$tier3_oas))) {
        updateNumericInput(session, "tier3_oas", value = remainder)
      }
    }
  })
  
  # Prevent manual edits to tier3 when locked
  observe({
    shinyjs::runjs(sprintf(
      "document.getElementById('%s').querySelector('input').readOnly = %s;",
      "tier3_oas", ifelse(isTRUE(input$lock_tier3), "true", "false")
    ))
  })
  
  # Core optimization (deterministic greedy, vectorized)
  optimization_results <- eventReactive(input$optimize, {
    # Expected supporters found per visited OA in each tier
    tier_returns <- function(p_support) {
      input$voters_per_oa * (p_support / 100) * (input$response_rate / 100)
    }
    
    tiers <- data.frame(
      tier = c("Tier 1", "Tier 2", "Tier 3"),
      num_oas = c(input$tier1_oas, input$tier2_oas, input$tier3_oas),
      likelihood = c(input$tier1_likelihood, input$tier2_likelihood, input$tier3_likelihood),
      stringsAsFactors = FALSE
    )
    tiers$return_per_visit <- tier_returns(tiers$likelihood)
    
    # Sort by return per visit (desc) for optimal filling
    ord <- order(tiers$return_per_visit, decreasing = TRUE)
    tiers_sorted <- tiers[ord, ]
    
    remaining <- input$total_visits
    visits_allocated_sorted <- pmin(tiers_sorted$num_oas, pmax(0, remaining))
    # But we must allocate sequentially respecting remaining
    for (i in seq_len(nrow(tiers_sorted))) {
      visits_allocated_sorted[i] <- min(tiers_sorted$num_oas[i], remaining)
      remaining <- remaining - visits_allocated_sorted[i]
    }
    
    tiers_sorted$visits_allocated <- visits_allocated_sorted
    tiers_sorted$total_voters_found <- tiers_sorted$visits_allocated * tiers_sorted$return_per_visit
    
    # Put back in original tier order for display
    tiers_final <- merge(tiers, tiers_sorted[, c("tier", "visits_allocated", "total_voters_found")],
                         by = "tier", sort = FALSE)
    # Ensure original row order
    tiers_final <- tiers_final[match(c("Tier 1","Tier 2","Tier 3"), tiers_final$tier), ]
    
    total_capacity <- sum(tiers$num_oas)
    total_used <- sum(tiers_final$visits_allocated)
    total_found <- sum(tiers_final$total_voters_found)
    
    status <- if (input$total_visits > total_capacity) {
      sprintf("Warning: Total visits (%d) exceed OA capacity (%d). %d visits unallocatable.",
              input$total_visits, total_capacity, input$total_visits - total_capacity)
    } else if (input$total_visits < total_capacity && isTRUE(input$lock_tier3)) {
      # Common case when lock is ON: capacity equals exactly total_visits
      "OK"
    } else {
      "OK"
    }
    
    list(
      tiers = tiers_final,
      total_visits_used = total_used,
      total_voters_found = total_found,
      total_capacity = total_capacity,
      status = status
    )
  })
  
  # Results table
  output$results_table <- DT::renderDataTable({
    results <- optimization_results()
    
    display_table <- results$tiers
    display_table$likelihood <- paste0(display_table$likelihood, "%")
    display_table$return_per_visit <- round(
      input$voters_per_oa * (as.numeric(sub("%","",display_table$likelihood))/100) * (input$response_rate/100), 2
    )
    display_table$total_voters_found <- round(display_table$total_voters_found, 0)
    
    # Add convenience metric: Avg visits per OA requested for that tier (visits / num_oas)
    display_table$avg_visits_per_oa <- ifelse(display_table$num_oas > 0,
                                              round(display_table$visits_allocated / display_table$num_oas, 2), 0)
    
    colnames(display_table) <- c("Tier", "Number of OAs", "Reform Likelihood",
                                 "Visits Allocated", "Reform Voters per Visit",
                                 "Total Reform Voters Found", "Avg Visits per OA")
    
    datatable(display_table, options = list(pageLength = 10, dom = 't'))
  })
  
  # Summary statistics
  output$summary_stats <- renderText({
    results <- optimization_results()
    eff <- if (input$total_visits > 0) 100 * results$total_visits_used / input$total_visits else 0
    
    paste0(
      "Status: ", results$status, "\n",
      "Total Visits Allocated: ", results$total_visits_used, " / ", input$total_visits, "\n",
      "Total Reform Voters (expected): ", round(results$total_voters_found, 0), "\n",
      "Average Return per Visit: ",
      if (results$total_visits_used > 0)
        sprintf("%.2f voters", results$total_voters_found / results$total_visits_used)
      else "0", "\n",
      "Efficiency (Allocated/Available): ", sprintf("%.1f%%", eff)
    )
  })
  
  # Allocation visualization
  output$allocation_plot <- renderPlotly({
    results <- optimization_results()
    
    plot_ly(data = results$tiers,
            x = ~tier, y = ~visits_allocated, type = 'bar',
            text = ~paste("Visits:", visits_allocated,
                          "<br>Reform Voters Found:", round(total_voters_found, 0)),
            textposition = 'outside') %>%
      layout(title = "Visit Allocation by Tier",
             xaxis = list(title = "Tier"),
             yaxis = list(title = "Number of Visits"),
             showlegend = FALSE)
  })
  
  # Returns per visit plot
  output$returns_plot <- renderPlotly({
    tiers <- optimization_results()$tiers
    # Recompute return_per_visit for clarity
    rpv <- input$voters_per_oa * (tiers$likelihood / 100) * (input$response_rate / 100)
    
    plot_ly(data = data.frame(tier = tiers$tier, rpv = rpv),
            x = ~tier, y = ~rpv, type = 'bar',
            text = ~paste("Return per Visit:", round(rpv, 2)),
            textposition = 'outside') %>%
      layout(title = "Expected Reform Voters Discovered per Visit",
             xaxis = list(title = "Tier"),
             yaxis = list(title = "Reform Voters per Visit"),
             showlegend = FALSE)
  })
  
  # Marginal returns analysis (using deterministic greedy allocation path)
  output$marginal_plot <- renderPlotly({
    res <- optimization_results()
    
    # Build the marginal sequence by simulating incremental visits with the same greedy rule
    tiers <- res$tiers
    tiers$rpv <- input$voters_per_oa * (tiers$likelihood / 100) * (input$response_rate / 100)
    
    # Order by rpv desc
    ord <- order(tiers$rpv, decreasing = TRUE)
    tiers_sorted <- tiers[ord, c("tier","num_oas","rpv")]
    
    # Create the list of marginal returns: repeat each tier's rpv as many times as its allocated visits (greedy)
    remaining <- input$total_visits
    mlist <- numeric(0)
    for (i in seq_len(nrow(tiers_sorted))) {
      take <- min(tiers_sorted$num_oas[i], remaining)
      if (take > 0) {
        mlist <- c(mlist, rep(tiers_sorted$rpv[i], take))
        remaining <- remaining - take
      }
    }
    # Pad zeros if there were more visits requested than capacity
    if (remaining > 0) mlist <- c(mlist, rep(0, remaining))
    
    cumulative <- cumsum(mlist)
    marginal_data <- data.frame(
      visit = seq_along(mlist),
      cumulative_voters = cumulative,
      marginal_return = mlist
    )
    
    plot_ly(data = marginal_data) %>%
      add_trace(x = ~visit, y = ~cumulative_voters, type = 'scatter', mode = 'lines',
                name = 'Cumulative Voters Found', yaxis = 'y') %>%
      add_trace(x = ~visit, y = ~marginal_return, type = 'scatter', mode = 'lines',
                name = 'Marginal Return per Visit', yaxis = 'y2') %>%
      layout(title = "Cumulative and Marginal Returns",
             xaxis = list(title = "Visit Number"),
             yaxis = list(title = "Cumulative Reform Voters Found"),
             yaxis2 = list(title = "Marginal Return", overlaying = 'y', side = 'right'),
             legend = list(x = 0.7, y = 0.9))
  })
}

# Needed for the readOnly toggle of tier3 when locked
if (!requireNamespace("shinyjs", quietly = TRUE)) install.packages("shinyjs")
library(shinyjs)

# Run the application
shinyApp(ui = ui, server = server)
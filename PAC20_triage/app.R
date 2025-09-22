library(shiny)
library(DT)
library(plotly)

tier_cols <- c("Tier 1" = "#1f77b4", "Tier 2" = "#ff7f0e", "Tier 3" = "#2ca02c")

ui <- fluidPage(
  titlePanel("Political Canvassing Resource Optimization Tool"),
  sidebarLayout(
    sidebarPanel(
      h3("Input Parameters"),
      numericInput("total_visits", "Total Visits Available:", value = 400, min = 1, step = 1),
      numericInput("voters_per_oa", "Voters per Output Area:", value = 300, min = 1, step = 1),
      numericInput("response_rate", "Canvassing Response Rate (%):", value = 20, min = 0.1, max = 100, step = 0.1),
      
      hr(),
      h4("Tier 1 (Highest Priority)"),
      numericInput("tier1_oas", "Number of OAs:", value = 107, min = 0, step = 1),
      numericInput("tier1_likelihood", "Reform Support Likelihood (%):", value = 45, min = 0, max = 100, step = 0.1),
      
      h4("Tier 2 (Medium Priority)"),
      numericInput("tier2_oas", "Number of OAs:", value = 107, min = 0, step = 1),
      numericInput("tier2_likelihood", "Reform Support Likelihood (%):", value = 33, min = 0, max = 100, step = 0.1),
      
      h4("Tier 3 (Lowest Priority)"),
      checkboxInput("lock_t3", "Set Tier 3 OAs as remainder so T1+T2+T3 = Total Visits", TRUE),
      numericInput("tier3_oas", "Number of OAs:", value = 106, min = 0, step = 1),
      numericInput("tier3_likelihood", "Reform Support Likelihood (%):", value = 15, min = 0, max = 100, step = 0.1),
      
      hr(),
      actionButton("optimize", "Optimize Allocation", class = "btn-primary")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Optimization Results",
                 br(),
                 h3("Optimal Visit Allocation"),
                 DTOutput("results_table"),
                 br(),
                 h3("Summary Statistics"),
                 verbatimTextOutput("summary_stats"),
                 br(),
                 h3("Resource Allocation Visualization"),
                 plotlyOutput("allocation_plot")
        ),
        tabPanel("Sensitivity Analysis",
                 br(),
                 h3("First-Pass Returns per Visit by Tier"),
                 plotlyOutput("returns_plot"),
                 br(),
                 h3("Marginal Returns (Selected Top-V Sequence)"),
                 plotlyOutput("marginal_plot")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Keep Tier 3 as remainder if requested
  observeEvent(list(input$total_visits, input$tier1_oas, input$tier2_oas, input$lock_t3), {
    if (isTRUE(input$lock_t3)) {
      remainder <- max(0, input$total_visits - input$tier1_oas - input$tier2_oas)
      if (!identical(remainder, input$tier3_oas)) {
        updateNumericInput(session, "tier3_oas", value = remainder)
      }
    }
  }, ignoreInit = TRUE)
  
  # Core optimizer: select top-V marginal returns with diminishing per-OA returns
  optimization_results <- eventReactive(input$optimize, {
    V  <- as.integer(input$total_visits)
    N  <- as.numeric(input$voters_per_oa)
    r  <- as.numeric(input$response_rate)/100
    
    tiers <- data.frame(
      tier       = c("Tier 1","Tier 2","Tier 3"),
      num_oas    = c(input$tier1_oas, input$tier2_oas, input$tier3_oas),
      likelihood = c(input$tier1_likelihood, input$tier2_likelihood, input$tier3_likelihood),
      stringsAsFactors = FALSE
    )
    
    # Supporters per OA in each tier
    tiers$S <- N * (tiers$likelihood/100)
    
    total_oas <- sum(tiers$num_oas)
    if (total_oas == 0 || V == 0 || r <= 0) {
      tiers$visits_allocated <- 0
      tiers$total_voters_found <- 0
      return(list(
        tiers = tiers, selected_sequence = numeric(0),
        per_tier_k = list("Tier 1"=integer(0),"Tier 2"=integer(0),"Tier 3"=integer(0)),
        total_visits_used = 0, total_voters_found = 0, status = "No capacity or zero response rate."
      ))
    }
    
    # How many passes do we need available? (ensure >= V slots in total)
    Kmax <- max(1, ceiling(V / max(1, total_oas)) + 2)  # a little headroom
    # Precompute marginal for pass k (k=1..Kmax) per tier
    # marginal_{t,k} = S_t * r * (1-r)^(k-1); handle r==1 safely
    pow_vec <- if (r < 1) (1 - r)^(0:(Kmax-1)) else c(1, rep(0, Kmax-1))
    
    # Build rows (WITHOUT replicating per-OA). Each row has capacity = num_oas.
    slots <- do.call(rbind, lapply(1:nrow(tiers), function(i) {
      data.frame(
        tier = tiers$tier[i],
        k    = 1:Kmax,
        marginal = tiers$S[i] * r * pow_vec,
        capacity = tiers$num_oas[i]
      )
    }))
    
    # Sort by marginal desc (higher returns first); tie-break on lower k first
    slots <- slots[order(-slots$marginal, slots$k), ]
    
    # Allocate visits across these rows with stacking constraint:
    # For a given tier, cannot allocate more k-th visits than (# of (k-1)-th visits allocated).
    # Track how many "at least k" visits we have per tier.
    tiers_k_alloc <- lapply(setNames(tiers$tier, tiers$tier), function(x) integer(Kmax))
    remaining <- V
    selected_sequence <- numeric(0)  # the ordered marginal returns actually chosen (for plotting)
    
    for (row_i in seq_len(nrow(slots))) {
      if (remaining <= 0) break
      tr  <- slots$tier[row_i]
      kk  <- slots$k[row_i]
      cap <- slots$capacity[row_i]
      
      # Max we can allocate for this row
      if (kk == 1) {
        already1 <- tiers_k_alloc[[tr]][1]
        cap <- max(0, cap - already1)
      } else {
        cap <- min(cap, tiers_k_alloc[[tr]][kk-1] - tiers_k_alloc[[tr]][kk])
        cap <- max(0, cap)
      }
      
      if (cap <= 0) next
      
      take <- min(cap, remaining)
      if (take > 0) {
        tiers_k_alloc[[tr]][kk] <- tiers_k_alloc[[tr]][kk] + take
        remaining <- remaining - take
        # Append 'take' copies of this marginal to the sequence
        if (slots$marginal[row_i] > 0) {
          selected_sequence <- c(selected_sequence, rep(slots$marginal[row_i], take))
        } else {
          # zeros add nothing; still fill sequence for length correctness
          selected_sequence <- c(selected_sequence, rep(0, take))
        }
      }
    }
    
    # Summaries per tier
    visits_allocated <- sapply(tiers$tier, function(tr) sum(tiers_k_alloc[[tr]]))
    voters_found <- sapply(tiers$tier, function(tr) {
      mks <- tiers$S[tiers$tier==tr] * r * pow_vec
      sum(tiers_k_alloc[[tr]] * mks)
    })
    
    tiers$visits_allocated    <- as.integer(visits_allocated)
    tiers$total_voters_found  <- as.numeric(voters_found)
    
    list(
      tiers = tiers,
      per_tier_k = tiers_k_alloc,
      selected_sequence = selected_sequence,
      total_visits_used = sum(visits_allocated),
      total_voters_found = sum(voters_found),
      status = if (remaining > 0) sprintf("Note: %d visits unallocatable (no marginal benefit left or no capacity).", remaining) else "OK"
    )
  })
  
  # Results table
  output$results_table <- renderDT({
    res <- optimization_results()
    df <- res$tiers
    df$`Reform Likelihood` <- paste0(round(df$likelihood,1), "%")
    df$`Visits Allocated`  <- df$visits_allocated
    df$`Total Reform Voters (expected)` <- round(df$total_voters_found, 1)
    
    # First-pass return per visit (for reference)
    first_pass_rpv <- df$S * (input$response_rate/100)
    df$`First-Pass RPV` <- round(first_pass_rpv, 2)
    
    out <- df[, c("tier","num_oas","Reform Likelihood","Visits Allocated",
                  "First-Pass RPV","Total Reform Voters (expected)")]
    colnames(out)[1:2] <- c("Tier","Number of OAs")
    datatable(out, options = list(pageLength = 10, dom = 't'))
  })
  
  # Summary
  output$summary_stats <- renderText({
    res <- optimization_results()
    eff <- if (input$total_visits > 0) 100*res$total_visits_used/input$total_visits else 0
    paste0(
      "Status: ", res$status, "\n",
      "Total Visits Allocated: ", res$total_visits_used, " / ", input$total_visits, "\n",
      "Total Reform Voters (expected): ", round(res$total_voters_found, 1), "\n",
      "Average Return per Visit: ",
      if (res$total_visits_used > 0) sprintf("%.2f", res$total_voters_found/res$total_visits_used) else "0.00",
      " voters\n",
      "Efficiency (Allocated/Available): ", sprintf("%.1f%%", eff)
    )
  })
  
  # Allocation bar (keep tier colours)
  output$allocation_plot <- renderPlotly({
    res <- optimization_results()
    df <- res$tiers
    plot_ly(df,
            x = ~tier, y = ~visits_allocated, type = 'bar',
            text = ~paste("Visits:", visits_allocated,
                          "<br>Expected voters:", round(total_voters_found,1)),
            textposition = 'outside',
            marker = list(color = tier_cols[df$tier])) %>%
      layout(title = "Visit Allocation by Tier",
             xaxis = list(title = "Tier"),
             yaxis = list(title = "Number of Visits"),
             showlegend = FALSE)
  })
  
  # First-pass RPV per tier (colours preserved)
  output$returns_plot <- renderPlotly({
    df <- optimization_results()$tiers
    first_pass_rpv <- df$S * (input$response_rate/100)
    plot_ly(data = data.frame(tier = df$tier, rpv = first_pass_rpv),
            x = ~tier, y = ~rpv, type = 'bar',
            text = ~paste("First-pass RPV:", round(rpv,2)),
            textposition = 'outside',
            marker = list(color = tier_cols[df$tier])) %>%
      layout(title = "Expected Reform Voters Discovered on First Visit",
             xaxis = list(title = "Tier"),
             yaxis = list(title = "Reform Voters per Visit"),
             showlegend = FALSE)
  })
  
  # Marginal sequence actually chosen (global optimal order)
  output$marginal_plot <- renderPlotly({
    seqv <- optimization_results()$selected_sequence
    if (length(seqv) == 0) seqv <- 0
    cumv <- cumsum(seqv)
    plot_ly() %>%
      add_trace(x = seq_along(cumv), y = cumv, type = 'scatter', mode = 'lines',
                name = 'Cumulative Expected Voters') %>%
      add_trace(x = seq_along(seqv), y = seqv, type = 'scatter', mode = 'lines',
                name = 'Marginal per Visit', yaxis = 'y2') %>%
      layout(title = "Cumulative and Marginal Returns (Optimal Visit Order)",
             xaxis = list(title = "Visit Number (global)"),
             yaxis = list(title = "Cumulative Expected Voters"),
             yaxis2 = list(title = "Marginal Return", overlaying = 'y', side = 'right'),
             legend = list(x = 0.7, y = 0.9))
  })
}

shinyApp(ui = ui, server = server)
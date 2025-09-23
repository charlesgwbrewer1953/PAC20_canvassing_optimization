# app.R
library(shiny)
library(DT)
library(plotly)
library(htmltools)

tier_cols <- c("Tier 1" = "#1f77b4", "Tier 2" = "#ff7f0e", "Tier 3" = "#2ca02c")

# Custom CSS to make the sidebar shrink to content width
custom_css <- tags$head(
  tags$style(HTML("
    /* Scope to our specific layout by ID */
    #app-layout > .row { display: flex; flex-wrap: nowrap; }
    #app-layout .col-sm-4 {
      float: none;
      flex: 0 0 auto;
      width: max-content;      /* shrink to longest line of text */
      max-width: 40vw;         /* safety cap for small screens */
    }
    #app-layout .col-sm-8 {
      float: none;
      flex: 1 1 auto;
      width: auto;             /* fill remaining space */
    }
  "))
)

ui <- fluidPage(
  titlePanel("Political Canvassing Resource Optimization Tool"),
  custom_css,
  div(
    id = "app-layout",
    sidebarLayout(
      sidebarPanel(
        h3("Input Parameters"),
        
        # Party selector
        selectInput(
          "party", "Select Party:",
          choices = c("Con", "Lab", "LD", "Ref", "Grn", "PC", "SNP"),
          selected = "Ref"
        ),
        
        numericInput("total_visits", "Total Visits Available:", value = 400, min = 1, step = 1),
        
        # Total number of OAs (Electoral Division)
        numericInput("total_oas", "Total number of OAs in Electoral Division:", value = 320, min = 1, step = 1),
        
        numericInput("voters_per_oa", "Voters per Output Area:", value = 300, min = 1, step = 1),
        numericInput("response_rate", "Canvassing Response Rate (%):", value = 20, min = 0.1, max = 100, step = 0.1),
        
        hr(),
        h4("Tier 1 (Highest Priority)"),
        numericInput("tier1_oas", "Number of OAs in Tier 1:", value = 107, min = 0, step = 1),
        numericInput("tier1_likelihood", "Support Likelihood (%):", value = 45, min = 0, max = 100, step = 0.1),
        
        h4("Tier 2 (Medium Priority)"),
        numericInput("tier2_oas", "Number of OAs in Tier 2:", value = 107, min = 0, step = 1),
        numericInput("tier2_likelihood", "Support Likelihood (%):", value = 33, min = 0, max = 100, step = 0.1),
        
        h4("Tier 3 (Lowest Priority)"),
        checkboxInput("lock_t3", "Set Tier 3 OAs as remainder so T1+T2+T3 = Total OAs", TRUE),
        numericInput("tier3_oas", "Number of OAs in Tier 3:", value = 106, min = 0, step = 1),
        numericInput("tier3_likelihood", "Support Likelihood (%):", value = 15, min = 0, max = 100, step = 0.1),
        
        hr(),
        h4("Pass Settings"),
        numericInput("max_passes_per_oa", "Max passes per OA (cap):", value = 5, min = 1, step = 1),
        
        hr(),
        actionButton("optimize", "Optimize Allocation", class = "btn-primary")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel(
            "Optimization Results",
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
          tabPanel(
            "Sensitivity Analysis",
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
)

server <- function(input, output, session) {
  
  # Keep Tier 3 as remainder of TOTAL OAs (not visits)
  observeEvent(list(input$total_oas, input$tier1_oas, input$tier2_oas, input$lock_t3), {
    if (isTRUE(input$lock_t3)) {
      remainder <- max(0, input$total_oas - input$tier1_oas - input$tier2_oas)
      if (!identical(remainder, input$tier3_oas)) {
        updateNumericInput(session, "tier3_oas", value = remainder)
      }
    }
  }, ignoreInit = TRUE)
  
  # ========================
  # Optimizer (eventReactive)
  # ========================
  optimization_results <- eventReactive(input$optimize, {
    V    <- as.integer(input$total_visits)
    Otot <- as.integer(input$total_oas)
    N    <- as.numeric(input$voters_per_oa)
    r    <- as.numeric(input$response_rate) / 100
    Kcap <- as.integer(max(1, input$max_passes_per_oa))
    
    tiers <- data.frame(
      tier       = c("Tier 1","Tier 2","Tier 3"),
      num_oas    = pmax(0L, c(input$tier1_oas, input$tier2_oas, input$tier3_oas)),
      likelihood = c(input$tier1_likelihood, input$tier2_likelihood, input$tier3_likelihood),
      stringsAsFactors = FALSE
    )
    
    # Consistency note
    sum_oas <- sum(tiers$num_oas)
    status_prefix <- if (sum_oas != Otot)
      sprintf("Note: Tier OAs total (%d) != Total OAs (%d). Using entered tier values. ", sum_oas, Otot)
    else "OK. "
    
    # Expected supporters per OA per tier
    tiers$S <- N * (tiers$likelihood / 100)
    
    total_oas <- sum(tiers$num_oas)
    if (total_oas == 0 || V == 0 || r <= 0) {
      tiers$visits_allocated <- 0L
      tiers$total_voters_found <- 0
      pass_dist <- setNames(vector("list", 3), tiers$tier)
      for (tr in tiers$tier) pass_dist[[tr]] <- integer(Kcap + 1)
      return(list(
        tiers = tiers,
        pass_distributions = pass_dist,
        selected_sequence = numeric(0),
        total_visits_used = 0,
        total_voters_found = 0,
        status = paste0(status_prefix, "No capacity or zero response rate."),
        Kmax = Kcap,
        Otot = Otot
      ))
    }
    
    Kmax <- Kcap
    pow_vec <- if (r < 1) (1 - r)^(0:(Kmax - 1)) else c(1, rep(0, Kmax - 1))
    
    # Build pass "slots" per tier; each pass k has capacity = number of OAs in that tier
    slots <- do.call(rbind, lapply(1:nrow(tiers), function(i) {
      data.frame(
        tier     = tiers$tier[i],
        k        = 1:Kmax,
        marginal = tiers$S[i] * r * pow_vec,   # expected new supporters on k-th pass
        capacity = tiers$num_oas[i]
      )
    }))
    
    # Sort slots by marginal return (desc), break ties by lower pass k
    slots <- slots[order(-slots$marginal, slots$k), ]
    
    # Stacking constraint per tier: c_{t,k} <= c_{t,k-1}; with c_{t,0} = num_oas
    per_tier_k <- lapply(setNames(tiers$tier, tiers$tier), function(x) integer(Kmax))
    remaining <- V
    selected_sequence <- numeric(0)
    
    for (row_i in seq_len(nrow(slots))) {
      if (remaining <= 0) break
      tr  <- slots$tier[row_i]
      kk  <- slots$k[row_i]
      cap <- slots$capacity[row_i]
      
      if (kk == 1) {
        cap <- max(0, cap - per_tier_k[[tr]][1])
      } else {
        cap <- min(cap, per_tier_k[[tr]][kk - 1] - per_tier_k[[tr]][kk])
        cap <- max(0, cap)
      }
      
      if (cap <= 0) next
      take <- min(cap, remaining)
      if (take > 0) {
        per_tier_k[[tr]][kk] <- per_tier_k[[tr]][kk] + take
        remaining <- remaining - take
        selected_sequence <- c(selected_sequence, rep(slots$marginal[row_i], take))
      }
    }
    
    # Summaries per tier
    visits_allocated <- sapply(tiers$tier, function(tr) sum(per_tier_k[[tr]]))
    voters_found <- sapply(tiers$tier, function(tr) {
      mks <- tiers$S[tiers$tier == tr] * r * pow_vec
      sum(per_tier_k[[tr]] * mks)
    })
    
    # Distributions: exactly n passes per OA
    pass_distributions <- setNames(vector("list", length(tiers$tier)), tiers$tier)
    max_passes_used <- setNames(integer(length(tiers$tier)), tiers$tier)
    avg_passes <- setNames(numeric(length(tiers$tier)), tiers$tier)
    
    for (tr in tiers$tier) {
      ck <- per_tier_k[[tr]]
      exact <- integer(Kmax + 1)  # bins 0..Kmax
      exact[1] <- tiers$num_oas[tiers$tier == tr] - ck[1]      # exactly 0 passes
      for (kk in 1:(Kmax - 1)) exact[kk + 1] <- ck[kk] - ck[kk + 1]
      exact[Kmax + 1] <- ck[Kmax]
      pass_distributions[[tr]] <- exact
      max_passes_used[tr] <- max(which(exact[-1] > 0), 0)
      nn <- 0:Kmax
      if (tiers$num_oas[tiers$tier == tr] > 0) {
        avg_passes[tr] <- sum(nn * exact) / tiers$num_oas[tiers$tier == tr]
      } else {
        avg_passes[tr] <- 0
      }
    }
    
    tiers$visits_allocated    <- as.integer(visits_allocated)
    tiers$total_voters_found  <- as.numeric(voters_found)
    tiers$avg_passes_per_oa   <- as.numeric(avg_passes[tiers$tier])
    tiers$max_passes_used     <- as.integer(max_passes_used[tiers$tier])
    
    list(
      tiers = tiers,
      pass_distributions = pass_distributions,
      selected_sequence = selected_sequence,
      total_visits_used = sum(visits_allocated),
      total_voters_found = sum(voters_found),
      status = paste0(
        status_prefix,
        if (remaining > 0) sprintf("%d visits unallocatable (no capacity or zero marginal).", remaining) else "OK"
      ),
      Kmax = Kmax,
      Otot = Otot
    )
  })
  
  # ========================
  # Outputs (use req(res))
  # ========================
  
  output$results_table <- renderDT({
    res <- optimization_results(); req(res)
    df <- res$tiers
    party <- input$party
    
    # Pretty distribution like "0:12, 1:95, 2:30, 3:0, ..."
    dist_str <- vapply(df$tier, function(tr) {
      exact <- res$pass_distributions[[tr]]
      paste0(paste0(0:res$Kmax, ":", exact), collapse = ", ")
    }, character(1))
    
    out <- data.frame(
      Tier = df$tier,
      "Number of OAs" = df$num_oas,
      "Support Likelihood" = paste0(round(df$likelihood, 1), "%"),
      "Visits Allocated (total)" = df$visits_allocated,
      "Avg Passes/OA" = round(df$avg_passes_per_oa, 2),
      "Max Passes Used" = df$max_passes_used,
      "Voters per Visit (First Pass)" = round(df$S * (input$response_rate / 100), 2),
      "Total Voters (expected)" = round(df$total_voters_found, 1),
      "Pass Distribution (passes:number of OAs)" = dist_str,
      check.names = FALSE
    )
    
    datatable(
      out,
      rownames = FALSE,  # remove unnamed first column
      options = list(pageLength = 10, dom = 't'),
      caption = tags$caption(
        style = 'caption-side: top; text-align: left; font-weight:600;',
        paste("Party:", party)
      )
    )
  })
  
  output$summary_stats <- renderText({
    res <- optimization_results(); req(res)
    party <- input$party
    eff <- if (input$total_visits > 0) 100 * res$total_visits_used / input$total_visits else 0
    
    avg_return_str <- if (res$total_visits_used > 0) {
      sprintf("%.2f %s voters", res$total_voters_found / res$total_visits_used, party)
    } else {
      "0.00"
    }
    
    paste0(
      "Status: ", res$status, "\n",
      "Total OAs (Electoral Division): ", res$Otot, "\n",
      "Tier OAs sum: ", sum(res$tiers$num_oas), "\n",
      "Total Visits Allocated: ", res$total_visits_used, " / ", input$total_visits, "\n",
      "Total ", party, " Voters (expected): ", round(res$total_voters_found, 1), "\n",
      "Average Return per Visit: ", avg_return_str, "\n",
      "Efficiency (Allocated/Available): ", sprintf("%.1f%%", eff)
    )
  })
  
  # Allocation bar (no label cutoff)
  output$allocation_plot <- renderPlotly({
    res <- optimization_results(); req(res)
    df <- res$tiers
    party <- input$party
    ymax <- if (nrow(df)) max(df$visits_allocated, na.rm = TRUE) else 1
    pad  <- if (ymax > 0) ymax * 0.2 else 1
    
    plot_ly(
      df,
      x = ~tier, y = ~visits_allocated, type = 'bar',
      text = ~paste(
        "Visits:", visits_allocated,
        "<br>Expected ", party, " voters:", round(total_voters_found, 1),
        "<br>Avg passes/OA:", round(avg_passes_per_oa, 2)
      ),
      textposition = 'outside',
      marker = list(color = tier_cols[df$tier]),
      cliponaxis = FALSE
    ) %>%
      layout(
        title = paste("Visit Allocation by Tier — Party:", party),
        xaxis = list(title = "Tier"),
        yaxis = list(title = "Number of Visits", range = c(0, ymax + pad)),
        margin = list(t = 90),
        showlegend = FALSE
      )
  })
  
  # First-pass returns per visit (no cutoff)
  output$returns_plot <- renderPlotly({
    res <- optimization_results(); req(res)
    df <- res$tiers
    party <- input$party
    first_pass_rpv <- df$S * (input$response_rate / 100)
    ymax <- if (length(first_pass_rpv)) max(first_pass_rpv, na.rm = TRUE) else 1
    pad  <- if (ymax > 0) ymax * 0.2 else 1
    
    plot_ly(
      data = data.frame(tier = df$tier, rpv = first_pass_rpv),
      x = ~tier, y = ~rpv, type = 'bar',
      text = ~paste("First-pass ", party, " voters/visit:", round(rpv, 2)),
      textposition = 'outside',
      marker = list(color = tier_cols[df$tier]),
      cliponaxis = FALSE
    ) %>%
      layout(
        title = paste("Expected ", party, " Voters per Visit (First Pass)", sep = ""),
        xaxis = list(title = "Tier"),
        yaxis = list(
          title = paste(party, " Voters per Visit"),
          range = c(0, ymax + pad)
        ),
        margin = list(t = 90),
        showlegend = FALSE
      )
  })
  
  # Marginal sequence chosen (global optimal)
  output$marginal_plot <- renderPlotly({
    res <- optimization_results(); req(res)
    party <- input$party
    seqv <- res$selected_sequence
    if (length(seqv) == 0) seqv <- 0
    cumv <- cumsum(seqv)
    
    plot_ly() %>%
      add_trace(
        x = seq_along(cumv), y = cumv, type = 'scatter', mode = 'lines',
        name = paste("Cumulative Expected", party, "Voters")
      ) %>%
      add_trace(
        x = seq_along(seqv), y = seqv, type = 'scatter', mode = 'lines',
        name = 'Marginal per Visit', yaxis = 'y2'
      ) %>%
      layout(
        title = paste("Cumulative and Marginal Returns — Party:", party),
        xaxis = list(title = "Visit Number (global)"),
        yaxis = list(title = paste("Cumulative Expected", party, "Voters")),
        yaxis2 = list(title = "Marginal Return", overlaying = 'y', side = 'right'),
        legend = list(x = 0.7, y = 0.9)
      )
  })
}

shinyApp(ui = ui, server = server)
labor_server <- function(input, output, session) {
  ns <- session$ns
  tabla <- readRDS("data/non_salary/bonuses_and_benefits_component.rds")
  
  # ---- Non Salary Tables and adding tenure  ----
  df_non_salary <- readRDS("data/non_salary/1. total_non_salary_costs.rds")
  # df_non_salary$tenure <- "all Years"
  # df_non_salary <- rbind(df_non_salary, df_non_salary_t)
  
  
  # non_salary variables
  ns_variables<-reactiveValues(
    order_country=NULL,
    country_sel="All",
    countries=c("All",unique(df_non_salary$country)),
    df_final=NULL
  )
  
  # --- Reading Tenure Variables --- #
  # df_non_salary_t <- readRDS("data/non_salary/total_non_salary_costs_tenure.rds")
  # df_non_salary_payer_t <- readRDS("data/non_salary/total_ns_costs_by_payer_tenure.rds")
  # df_non_salary_component_t <- readRDS("data/non_salary/total_ns_costs_by_component_tenure.rds")
  
  
  df_non_salary_payer <- readRDS("data/non_salary/2. total_ns_costs_by_payer.rds")
  # df_non_salary_payer$tenure <- "all Years"
  # df_non_salary_payer <- rbind(df_non_salary_payer, df_non_salary_payer_t)
  
  df_non_salary_component <- readRDS("data/non_salary/3. total_ns_costs_by_component.rds")
  # df_non_salary_component$tenure <- "all Years"
  # df_non_salary_component <- rbind(df_non_salary_component, df_non_salary_component_t)
  
  # ---- Selection Groups: Button results ----
  selected_groupA <- reactiveVal("total") # Total, by Payer, By Component   
  selected_groupB <- reactiveVal("1sm") # 1 MW / 2 MW / 5 MW / 10 MW / 15 MW
  selected_groupC <- reactiveVal("all_component")
  selected_groupD <- reactiveVal("all_bonuses")
  
  # ---- Slider ----
  # tenure_selected <- reactiveVal("all Years")
  
  # ---- Check if the user is selecting "all Years"
  # tenure_selected <- reactive({
  #   if (isTRUE(input$all_slider)) {
  #     return("all")   
  #   } else {
  #     req(input$tenure_slider)
  #     return(as.numeric(input$tenure_slider))
  #   }
  # })
  
  output$country_selection <- renderUI({
    div(
      class = "pretty-select",
      selectInput(
        inputId = ns("country_selection_user"),
        label = "Country Analysis by:",
        choices = ns_variables$countries,
        selected = "All"
      )
    )
  })
  
  
  # ---- First Selection ----
  observeEvent(input$btn_total,  { selected_groupA("total") })
  observeEvent(input$btn_payer,  { selected_groupA("payer") })
  observeEvent(input$btn_component,  { 
    selected_groupA("component") 
  })
  observeEvent(input$country_selection_user,  { 
    ns_variables$country_sel=input$country_selection_user
  })
  
  
  # ---- MW Selection ----
  observeEvent(input$btn_sm1,  { selected_groupB("1sm") })
  observeEvent(input$btn_sm2,  { selected_groupB("2sm") })
  observeEvent(input$btn_sm5,  { selected_groupB("5sm") })
  observeEvent(input$btn_sm10, { selected_groupB("10sm") })
  observeEvent(input$btn_sm15, { selected_groupB("15sm") })
  
  # ---- Components ----
  observeEvent(input$all_component,  { selected_groupC("all_component") })
  observeEvent(input$bonus,  { selected_groupC("bonuses_and_benefits") })
  observeEvent(input$pensions,  { selected_groupC("pensions") })
  observeEvent(input$payroll, { selected_groupC("payroll_taxes") })
  observeEvent(input$health, { selected_groupC("health") })
  observeEvent(input$occupational_risk, { selected_groupC("occupational_risk") })
  
  # ---- Bonuses and Benefits ----
  observeEvent(input$ab,  { selected_groupD("ab") })
  observeEvent(input$pl,  { selected_groupD("pl") })
  observeEvent(input$ob,  { selected_groupD("ob") })
  observeEvent(input$up,  { selected_groupD("up") })
  
  # ---- Graph ----
  output$plot <- renderPlotly({
    
    
    # Requirements
    req(selected_groupA())
    req(selected_groupB())
    
    # Results from user click
    groupA <- selected_groupA()
    groupB <- selected_groupB()
    groupC <- selected_groupC()
    groupD <- selected_groupD()
    #t <- tenure_selected()
    
    # # Checking values
    # print("Filtros")
    # print(paste("GroupA:", groupA))
    # print(paste("GroupB:", groupB))
    # print(paste("Tenure:", paste(t,"Years")))
    
    # Transform value from button "1sm" → "1 MW"
    wage_filter<-paste0(substr(groupB, 1, nchar(groupB) - 2), " MW")
    
    # # case 1 year
    # if(t==1){
    #   te=paste(t,"Year")
    # }
    # else{
    #   te=paste(t,"Years")
    # }
    
    # ---- Total ----
    if (groupA == "total" & ns_variables$country_sel=="All") {
      # Filtering total non salary
      df <- df_non_salary %>%
        dplyr::filter(
          wage == wage_filter
        )
      
      if (nrow(df) == 0) {
        showNotification("No Data for this combination.", type = "error")
        return(NULL)
      }
      
      df_wide <- df %>%
        tidyr::pivot_wider(
          names_from = type,
          values_from = value
        ) %>%
        arrange(t_min) %>%
        mutate(country = factor(country, levels = country))
      
      ns_variables$order_country <- unique(as.character(df_wide$country))
      
      ns_variables$df_final=df_wide
      p <- plot_ly(df_wide) %>%
        add_bars(
          x = ~country,
          y = ~t_min,
          name = "Minimum",
          hoverinfo = "text",
          text = ~paste0(
            "<b>Country:</b> ", country, "<br>",
            "<b>Minimum Cost:</b> ", t_min, "<br>",
            "<b>Wage Analysis:</b> Total - ", wage_filter, "<br>"
          ),
          textposition="none",
          marker = list(color = "#00C1FF")
        ) %>%
        add_bars(
          x = ~country,
          y = ~t_max,
          name = "Maximum",
          hoverinfo = "text",
          text = ~paste0(
            "<b>Country:</b> ", country, "<br>",
            "<b>Max Cost:</b> ", t_max, "<br>",
            "<b>Wage Analysis:</b> Total - ", wage_filter, "<br>"
          ),
          textposition="none",
          marker = list(color = "#002244")
        ) %>%
        layout(
          barmode = "group",
          margin = list(t = 50),
          showlegend=F,
          paper_bgcolor = "rgba(0,0,0,0)",   
          plot_bgcolor  = "rgba(0,0,0,0)",   
          
          xaxis = list(
            title = "",
            showgrid = FALSE,
            zeroline = FALSE,
            showline = FALSE
          ),
          yaxis = list(
            title = "Non-Salary costs as % of wages",
            showgrid = FALSE,
            zeroline = FALSE,
            showline = FALSE
          )
        )
      
      return(p)
    }
    if (groupA == "total" & ns_variables$country_sel!="All") {
      ns_variables$countries=c("All",unique(df_non_salary$country))
      
      # Filtering total non salary
      df <- df_non_salary %>%
        filter(
          wage == wage_filter,
          country==ns_variables$country_sel
        )
      
      if (nrow(df) == 0) {
        showNotification("No Data for this combination.", type = "error")
        return(NULL)
      }
      
      df_wide <- df %>%
        tidyr::pivot_wider(
          names_from = type,
          values_from = value
        ) %>%
        arrange(t_min) %>%
        mutate(country = factor(country, levels = country))
      
      ns_variables$df_final=df_wide
      
      p <- plot_ly(df_wide) %>%
        add_bars(
          x = ~country,
          y = ~t_min,
          name = "Minimum",
          hoverinfo = "text",
          text = ~paste0(
            "<b>Country:</b> ", country, "<br>",
            "<b>Minimum Cost:</b> ", t_min, "<br>",
            "<b>Wage Analysis:</b> Total - ", wage_filter, "<br>"
          ),
          textposition="none",
          marker = list(color = "#00C1FF")
        ) %>%
        add_bars(
          x = ~country,
          y = ~t_max,
          name = "Maximum",
          hoverinfo = "text",
          text = ~paste0(
            "<b>Country:</b> ", country, "<br>",
            "<b>Max Cost:</b> ", t_max, "<br>",
            "<b>Wage Analysis:</b> Total - ", wage_filter, "<br>"
          ),
          textposition="none",
          marker = list(color = "#002244")
        ) %>%
        layout(
          barmode = "group",
          margin = list(t = 50),
          showlegend=F,
          paper_bgcolor = "rgba(0,0,0,0)",   
          plot_bgcolor  = "rgba(0,0,0,0)",   
          
          xaxis = list(
            title = "",
            showgrid = FALSE,
            zeroline = FALSE,
            showline = FALSE
          ),
          yaxis = list(
            title = "Non-Salary costs as % of wages",
            showgrid = FALSE,
            zeroline = FALSE,
            showline = FALSE
          )
        )
      
      return(p)
    }
    
    # ---- Total By Payer ----
    if (groupA == "payer" & ns_variables$country_sel=="All") {
      ns_variables$countries=c("All",unique(df_non_salary$country))
      # Filtering total non salary by payer
      
      
      df_long <- df_non_salary_payer %>%
        filter(
          wage == wage_filter
        ) %>%
        select(country, type_by_payer, value) %>%
        mutate(
          group = ifelse(grepl("_min$", type_by_payer), "Min", "Max"),
          payer = ifelse(grepl("^st_er", type_by_payer), "Employer", "Employee"),
          group = factor(group, levels = c("Min", "Max"))
        )
      
      df_long <- df_long %>%
        mutate(country = factor(country, levels = ns_variables$order_country)) %>% 
        arrange(country)
      
      if (nrow(df_long) == 0) {
        showNotification("No Data for this combination.", type = "error")
        return(NULL)
      }
      
      
      df <- df_long
      df$Type <- factor(df$payer, levels = c("Employee", "Employer"))
      df$Scenario <- factor(df$group, levels = c("Min", "Max"))
      
      colors <- c("Employer" = "#002244", "Employee" = "#00C1FF")
      
      paises <- unique(df$country)
      plot_list <- list()
      
      ns_variables$df_final=df
      for (i in seq_along(paises)) {
        pais <- paises[i]
        data_pais <- df %>% filter(country == pais)

        if (nrow(data_pais) == 0) next
        
        show_legend <- ifelse(i == 1, TRUE, FALSE)
        
        p <- plot_ly(data_pais, x = ~Scenario, y = ~value, type = 'bar',
                     color = ~Type, colors = colors, legendgroup = ~Type,
                     showlegend = show_legend, text = ~value,
                     hoverinfo = "text+y+name") %>%
          layout(
            paper_bgcolor = "rgba(0,0,0,0)",   
            plot_bgcolor  = "rgba(0,0,0,0)",   
            xaxis = list(
              title = pais,
              showgrid = FALSE,
              zeroline = FALSE,
              showline = FALSE
            ),
            yaxis = list(
              title = ifelse(i == 1, "Non-salary costs as % of wages", ""),
              range = c(0, 140),
              showgrid = FALSE,
              zeroline = FALSE,
              showline = FALSE
            ),
            barmode = 'stack'
            #bargap = 0.1
          )
        
        plot_list[[i]] <- p
      }
      
      fig <- subplot(plot_list, nrows = 1, shareY = TRUE, titleX = TRUE) %>%
        layout(
          title = "",
          legend = list(orientation = "h", x = 0.4, y = -0.2),
          margin = list(b = 80)
        )
      
      return(fig)
    }
    if (groupA == "payer" & ns_variables$country_sel!="All") {
      ns_variables$countries=c("All",unique(df_non_salary$country))
      
      # Filtering total non salary by payer
      
      
      df_long <- df_non_salary_payer %>%
        filter(
          wage == wage_filter,
          country== ns_variables$country_sel
        ) %>%
        select(country, type_by_payer, value) %>%
        mutate(
          group = ifelse(grepl("_min$", type_by_payer), "Min", "Max"),
          payer = ifelse(grepl("^st_er", type_by_payer), "Employer", "Employee"),
          group = factor(group, levels = c("Min", "Max"))
        )
      
      
      if (nrow(df_long) == 0) {
        showNotification("No Data for this combination.", type = "error")
        return(NULL)
      }
      
      
      df <- df_long
      df$Type <- factor(df$payer, levels = c("Employee", "Employer"))
      df$Scenario <- factor(df$group, levels = c("Min", "Max"))
      
      colors <- c("Employer" = "#002244", "Employee" = "#00C1FF")
      
      paises <- unique(df$country)
      plot_list <- list()
      
      ns_variables$df_final=df
      for (i in seq_along(paises)) {
        pais <- paises[i]
        data_pais <- df %>% filter(country == pais)
        
        show_legend <- ifelse(i == 1, TRUE, FALSE)
        
        p <- plot_ly(data_pais, x = ~Scenario, y = ~value, type = 'bar',
                     color = ~Type, colors = colors, legendgroup = ~Type,
                     showlegend = show_legend, text = ~value,
                     hoverinfo = "text+y+name") %>%
          layout(
            paper_bgcolor = "rgba(0,0,0,0)",   
            plot_bgcolor  = "rgba(0,0,0,0)", 
            xaxis = list(
              title = pais,
              showgrid = FALSE,
              zeroline = FALSE,
              showline = FALSE
            ),
            yaxis = list(
              title = ifelse(i == 1, "Non-salary costs as % of wages", ""),
              range = c(0, 140),
              showgrid = FALSE,
              zeroline = FALSE,
              showline = FALSE
            ),
            barmode = 'stack'
          )
        
        plot_list[[i]] <- p
      }
      
      fig <- subplot(plot_list, nrows = 1, shareY = TRUE, titleX = TRUE) %>%
        layout(
          title = "",
          legend = list(orientation = "h", x = 0.4, y = -0.2),
          margin = list(b = 80)
        )
      
      return(fig)
    }
    
    # ---- Total by Component ----

    if (groupA == "component" & groupC=="all_component" & ns_variables$country_sel=="All") {
      ns_variables$countries=c("All",unique(df_non_salary$country))
      
      df_long <- df_non_salary_component %>%
        filter(
          wage == wage_filter
        ) %>%
        select(country, type_by_component, value) %>%
        mutate(
          group = ifelse(grepl("_min$", type_by_component), "Min", "Max"),
          payer = ifelse(grepl("^st_p", type_by_component), "Pension", 
                         ifelse(grepl("^st_h", type_by_component), "Health",
                                ifelse(grepl("^st_b", type_by_component), "Bonuses and Benefits",
                                       ifelse(grepl("^st_or", type_by_component), "Labor Risk","Payroll Taxes")))),
          group = factor(group, levels = c("Min", "Max"))
        )
      
      df_long <- df_long %>%
        mutate(country = factor(country, levels = ns_variables$order_country)) %>% 
        arrange(country)
      
      if (nrow(df_long) == 0) {
        showNotification("No Data for this combination.", type = "error")
        return(NULL)
      }
      
      
      df <- df_long
      df$Type <- factor(df$payer, levels = c("Pension", "Health","Labor Risk","Bonuses and Benefits","Payroll Taxes"))
      df$Scenario <- factor(df$group, levels = c("Min", "Max"))
      
      colors <- c("Pension"="#00C1FF","Health"="#002244",
                  "Labor Risk"="#FF8743","Bonuses and Benefits"="#B62650",
                  "Payroll Taxes"="#A162F7")
      
      paises <- unique(df$country)
      plot_list <- list()
      
      ns_variables$df_final=df
      for (i in seq_along(paises)) {
        pais <- paises[i]
        data_pais <- df %>% filter(country == pais)
        
        show_legend <- ifelse(i == 1, TRUE, FALSE)
        
        p <- plot_ly(data_pais, x = ~Scenario, y = ~value, type = 'bar',
                     color = ~Type, colors = colors, legendgroup = ~Type,
                     showlegend = show_legend, text = ~value,
                     hoverinfo = "text+y+name") %>%
          layout(
            paper_bgcolor = "rgba(0,0,0,0)",   
            plot_bgcolor  = "rgba(0,0,0,0)", 
            xaxis = list(
              title = pais,
              showgrid = FALSE,
              zeroline = FALSE,
              showline = FALSE
            ),
            yaxis = list(
              title = ifelse(i == 1, "Non-salary costs as % of wages", ""),
              range = c(0, 140),
              showgrid = FALSE,
              zeroline = FALSE,
              showline = FALSE
            ),
            barmode = 'stack'
          )
        
        plot_list[[i]] <- p
      }
      
      fig <- subplot(plot_list, nrows = 1, shareY = TRUE, titleX = TRUE) %>%
        layout(
          title = "",
          legend = list(orientation = "h", x = 0.3, y = -0.2),
          margin = list(b = 80)
        )
      
      return(fig)
    }
    if (groupA == "component" & groupC=="all_component" & ns_variables$country_sel!="All") {
      ns_variables$countries=c("All",unique(df_non_salary$country))
      
      df_long <- df_non_salary_component %>%
        filter(
          wage == wage_filter,
          country==ns_variables$country_sel
        ) %>%
        select(country, type_by_component, value) %>%
        mutate(
          group = ifelse(grepl("_min$", type_by_component), "Min", "Max"),
          payer = ifelse(grepl("^st_p", type_by_component), "Pension", 
                         ifelse(grepl("^st_h", type_by_component), "Health",
                                ifelse(grepl("^st_b", type_by_component), "Bonuses and Benefits",
                                       ifelse(grepl("^st_or", type_by_component), "Labor Risk","Payroll Taxes")))),
          group = factor(group, levels = c("Min", "Max"))
        )
      
      
      if (nrow(df_long) == 0) {
        showNotification("No Data for this combination.", type = "error")
        return(NULL)
      }
      
      
      df <- df_long
      df$Type <- factor(df$payer, levels = c("Pension", "Health","Labor Risk","Bonuses and Benefits","Payroll Taxes"))
      df$Scenario <- factor(df$group, levels = c("Min", "Max"))
      
      colors <- c("Pension"="#00C1FF","Health"="#002244",
                  "Labor Risk"="#FF8743","Bonuses and Benefits"="#B62650",
                  "Payroll Taxes"="#A162F7")
      
      paises <- unique(df$country)
      plot_list <- list()
      
      ns_variables$df_final=df
      for (i in seq_along(paises)) {
        pais <- paises[i]
        data_pais <- df %>% filter(country == pais)
        
        show_legend <- ifelse(i == 1, TRUE, FALSE)
        
        p <- plot_ly(data_pais, x = ~Scenario, y = ~value, type = 'bar',
                     color = ~Type, colors = colors, legendgroup = ~Type,
                     showlegend = show_legend, text = ~value,
                     hoverinfo = "text+y+name") %>%
          layout(
            paper_bgcolor = "rgba(0,0,0,0)",   
            plot_bgcolor  = "rgba(0,0,0,0)", 
            xaxis = list(
              title = pais,
              showgrid = FALSE,
              zeroline = FALSE,
              showline = FALSE
            ),
            yaxis = list(
              title = ifelse(i == 1, "Non-salary costs as % of wages", ""),
              range = c(0, 140),
              showgrid = FALSE,
              zeroline = FALSE,
              showline = FALSE
            ),
            barmode = 'stack'
          )
        
        plot_list[[i]] <- p
      }
      
      fig <- subplot(plot_list, nrows = 1, shareY = TRUE, titleX = TRUE) %>%
        layout(
          title = "",
          legend = list(orientation = "h", x = 0.3, y = -0.2),
          margin = list(b = 80)
        )
      
      return(fig)
    }
    
    if (groupA == "component" & groupC!="all_component" & ns_variables$country_sel=="All") {
      
      if(groupC=="bonuses_and_benefits" & groupD!="all_bonuses"){
        path_component=paste0("data/non_salary/",paste0(groupC,"_component.rds"))
        df=readRDS(path_component)
        df <- df %>%
          filter(
            wage == wage_filter,
            component == groupD
          ) %>%
          select(country, min_max_component, value) %>%
          mutate(
            type = ifelse(grepl("_min$", min_max_component), "Min", "Max")
          )
        ns_variables$countries=c("All",unique(df$country))
      }
      else{
        if(input$component_type=="Total"){
          path_component=paste0("data/non_salary/",paste0(groupC,"_all.rds"))
          df=readRDS(path_component)
          df <- df %>%
            filter(
              wage == wage_filter
            ) %>%
            select(country, min_max_total, value) %>%
            mutate(
              type = ifelse(grepl("_min$", min_max_total), "Min", "Max")
            )
          ns_variables$countries=c("All",unique(df$country))
        }
      }
      
      if (nrow(df) == 0) {
        showNotification("No Data for this combination.", type = "error")
        return(NULL)
      }
      
      df_wide=df %>%
        group_by(country) %>%
        summarize(
          t_min = min(value, na.rm = TRUE),
          t_max = max(value, na.rm = TRUE)
        )%>%
        arrange(t_min) %>%
        mutate(country = factor(country, levels = country))
      
      #ns_variables$order_country <- unique(as.character(df_wide$country))
      
      ns_variables$df_final=df_wide
      p <- plot_ly(df_wide) %>%
        add_bars(
          x = ~country,
          y = ~t_min,
          name = "Minimum",
          hoverinfo = "text",
          text = ~paste0(
            "<b>Country:</b> ", country, "<br>",
            "<b>Minimum Cost:</b> ", t_min, "<br>",
            "<b>Wage Analysis:</b> Total - ", wage_filter, "<br>"
          ),
          textposition="none",
          marker = list(color = "#00C1FF")
        ) %>%
        add_bars(
          x = ~country,
          y = ~t_max,
          name = "Maximum",
          hoverinfo = "text",
          text = ~paste0(
            "<b>Country:</b> ", country, "<br>",
            "<b>Max Cost:</b> ", t_max, "<br>",
            "<b>Wage Analysis:</b> Total - ", wage_filter, "<br>"
          ),
          textposition="none",
          marker = list(color = "#002244")
        ) %>%
        layout(
          barmode = "group",
          paper_bgcolor = "rgba(0,0,0,0)",   
            plot_bgcolor  = "rgba(0,0,0,0)", 
            xaxis = list(
              title = "",
              showgrid = FALSE,
              zeroline = FALSE,
              showline = FALSE
            ),
            yaxis = list(
              title = "",
              showgrid = FALSE,
              zeroline = FALSE,
              showline = FALSE
            ),
          margin = list(t = 50),
          showlegend=F
        )
      
      return(p)
    }
    if (groupA == "component" & groupC!="all_component" & ns_variables$country_sel!="All") {
      if(groupC=="bonuses_and_benefits" & groupD!="all_bonuses"){
        path_component=paste0("data/non_salary/",paste0(groupC,"_component.rds"))
        df=readRDS(path_component)
        df <- df %>%
          filter(
            wage == wage_filter,
            component == groupD,
            country==ns_variables$country_sel
          ) %>%
          select(country, min_max_component, value) %>%
          mutate(
            type = ifelse(grepl("_min$", min_max_component), "Min", "Max")
          )
      }
      else{
        if(input$component_type=="Total"){
          path_component=paste0("data/non_salary/",paste0(groupC,"_all.rds"))
          df=readRDS(path_component)
          df <- df %>%
            filter(
              wage == wage_filter,
              country==ns_variables$country_sel
            ) %>%
            select(country, min_max_total, value) %>%
            mutate(
              type = ifelse(grepl("_min$", min_max_total), "Min", "Max")
            )
        }
      }
      if (nrow(df) == 0) {
        showNotification("No Data for this combination.", type = "error")
        return(NULL)
      }
      
      
      df_wide=df %>%
        group_by(country) %>%
        summarize(
          t_min = min(value, na.rm = TRUE),
          t_max = max(value, na.rm = TRUE)
        )%>%
        arrange(t_min) %>%
        mutate(country = factor(country, levels = country))
      
      #ns_variables$order_country <- unique(as.character(df_wide$country))
      
      ns_variables$df_final=df_wide
      p <- plot_ly(df_wide) %>%
        add_bars(
          x = ~country,
          y = ~t_min,
          name = "Minimum",
          hoverinfo = "text",
          text = ~paste0(
            "<b>Country:</b> ", country, "<br>",
            "<b>Minimum Cost:</b> ", t_min, "<br>",
            "<b>Wage Analysis:</b> Total - ", wage_filter, "<br>"
          ),
          textposition="none",
          marker = list(color = "#00C1FF")
        ) %>%
        add_bars(
          x = ~country,
          y = ~t_max,
          name = "Maximum",
          hoverinfo = "text",
          text = ~paste0(
            "<b>Country:</b> ", country, "<br>",
            "<b>Max Cost:</b> ", t_max, "<br>",
            "<b>Wage Analysis:</b> Total - ", wage_filter, "<br>"
          ),
          textposition="none",
          marker = list(color = "#002244")
        ) %>%
        layout(
          barmode = "group",
          paper_bgcolor = "rgba(0,0,0,0)",   
          plot_bgcolor  = "rgba(0,0,0,0)", 
          xaxis = list(
            title = "",
            showgrid = FALSE,
            zeroline = FALSE,
            showline = FALSE
          ),
          yaxis = list(
            title = "",
            showgrid = FALSE,
            zeroline = FALSE,
            showline = FALSE
          ),
          margin = list(t = 50),
          showlegend=F
        )
      
      return(p)
    }
    
  })
  
  
  output$tabla_detalle<-reactable::renderReactable({
    groupA <- selected_groupA()
    groupC <- selected_groupC()
    groupD <- selected_groupD()
    con_sel=ns_variables$country_sel
    if(groupA!= "component" ) return()
    else{
      if(groupA== "component" & groupC=="all_component"){
        return()
      }
      else if (groupA== "component" & groupC=="bonuses_and_benefits" & groupD=="all_bonuses"){
        data <- read_excel("data/non_salary/tables.xlsx", sheet = "TL All B")
        data <- as.data.frame(data)
        if(con_sel!="All"){
          data=data %>% dplyr::filter(Country==con_sel)
        }
        
      }
      else if (groupA== "component" & groupC=="bonuses_and_benefits" & groupD=="ab"){
        
        data <- read_excel("data/non_salary/tables.xlsx", sheet = "TL ab")
        data <- as.data.frame(data)
        
        if(con_sel!="All"){
          data=data %>% dplyr::filter(Country==con_sel)
        }
      }
      else if (groupA== "component" & groupC=="bonuses_and_benefits" & groupD=="pl"){
        
        data <- read_excel("data/non_salary/tables.xlsx", sheet = "TL pl")
        data <- as.data.frame(data)
        
        if(con_sel!="All"){
          data=data %>% dplyr::filter(Country==con_sel)
        }
      }
      else if (groupA== "component" & groupC=="bonuses_and_benefits" & groupD=="up"){
        
        data <- read_excel("data/non_salary/tables.xlsx", sheet = "TL up")
        data <- as.data.frame(data)
        
        if(con_sel!="All"){
          data=data %>% dplyr::filter(Country==con_sel)
        }
      }
      else if (groupA== "component" & groupC=="bonuses_and_benefits" & groupD=="ob"){
        
        data <- read_excel("data/non_salary/tables.xlsx", sheet = "TL or")
        data <- as.data.frame(data)
        
        if(con_sel!="All"){
          data=data %>% dplyr::filter(Country==con_sel)
        }
      }
      else if (groupA== "component" & groupC=="health"){
  
        data <- read_excel("data/non_salary/tables.xlsx", sheet = "TL H")
        data <- as.data.frame(data)
        if(con_sel!="All"){
          data=data %>% dplyr::filter(Country==con_sel)
        }
      }
      else if (groupA== "component" & groupC=="payroll_taxes"){
  
        data <- read_excel("data/non_salary/tables.xlsx", sheet = "TL Pt")
        data <- as.data.frame(data)
        if(con_sel!="All"){
          data=data %>% dplyr::filter(Country==con_sel)
        }
      }
      else if (groupA== "component" & groupC=="pensions"){
  
        data <- read_excel("data/non_salary/tables.xlsx", sheet = "TL All P")
        data <- as.data.frame(data)
        if(con_sel!="All"){
          data=data %>% dplyr::filter(Country==con_sel)
        }
      }
    
    
    reactable::reactable(
      data,
      
      # Estilo general aplicado a todas las columnas
      defaultColDef = reactable::colDef(
        html = TRUE,
        minWidth = 140,
        maxWidth = 260,
        align = "left",
        style = list(
          whiteSpace = "normal",     # permite texto multilínea
          lineHeight = "1.35",
          fontSize = "12px",
          padding = "6px",
          textAlign = "justify"
        )
      ),
      
      bordered = TRUE,
      striped = TRUE,
      highlight = TRUE,
      resizable = TRUE,
      defaultPageSize = 8
    )
    
} 
    
  })
  
  
  # --- Components ----
  output$component_buttons <- renderUI({
    
    groupA <- selected_groupA()
    
    if (groupA != "component") {
      return(div(style="visibility:hidden;"))
    }
    div(
      class = "horizontal-container",
      style = "display:flex; align-items:center; justify-content:space-between; width:100%;",
      
      # ---- botones a la izquierda ----
      div(
        class = "component-buttons-container",
        style = "display:flex; flex-wrap:wrap; gap:8px;",
        
        actionButton(
          ns("all_component"),
          "All",
          class = "component-btn active"
        ),
        
        actionButton(
          ns("bonus"),
          "Bonuses and Benefits",
          class = "component-btn"
        ),
        
        actionButton(
          ns("pensions"),
          "Pension",
          class = "component-btn"
        ),

        actionButton(
          ns("health"),
          "Health",
          class = "component-btn"
        ),

        actionButton(
          ns("occupational_risk"),
          "Occupational Risk",
          class = "component-btn"
        ),
        
        actionButton(
          ns("payroll"),
          "Payroll Taxes",
          class = "component-btn"
        )
      ),
      
      # ---- select input a la derecha ----
      div(
        class = "pretty-select",
        style = "
          display:flex;
          flex-direction:column;
          min-width:180px;
        ",
        
        tags$label(
          "Analysis by:",
          style="font-size:12px; font-weight:bold; margin-bottom:4px;"
        ),
        
        selectInput(
          inputId = ns("component_type"),
          label = NULL,
          choices = c("Total"), # agregar Payer despues
          selected = "Total",
          width = "100%"
        )
      )
    )
  })
  
  output$bonus_buttons <- renderUI({
    
    groupC <- selected_groupC()
    groupA <- selected_groupA()
    
    if (groupC != "bonuses_and_benefits" & groupA !="component") {
      return(div(style="visibility:hidden;"))
    }
    if (groupC == "bonuses_and_benefits" & groupA =="component") {
      div(
        class = "horizontal-container",
        style = "display:flex; align-items:center; justify-content:space-between; width:100%;",
        
        # ---- título ----
        div(
          tags$div(
            "Bonuses and Benefits Components",
            style = "font-weight: bold; color: #b0b0b0; font-size: 14px; margin-bottom: 5px;"
          )
        ),
        
        # ---- botones a la izquierda ----
        div(
          class = "component-buttons-container",
          style = "display:flex; flex-wrap:wrap; gap:8px;",
          
          actionButton(
            ns("all_bonuses"),
            "All Bonuses",
            class = "component-btn active"
          ),
          
          actionButton(
            ns("ab"),
            "Annual and other bonuses",
            class = "component-btn"
          ),
          
          actionButton(
            ns("pl"),
            "Paid Leave",
            class = "component-btn"
          ),
          
          actionButton(
            ns("up"),
            "Unemployment Protection",
            class = "component-btn"
          ),
          
          actionButton(
            ns("ob"),
            "other bonuses and benefits",
            class = "component-btn"
          )
        )
      )
    }
  })
  
  
  
  output$download_df <- downloadHandler(
    filename = function() {
      paste0("data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(ns_variables$df_final, file, row.names = FALSE)
    },
    contentType = "text/csv"
  )
  
}


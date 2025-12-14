labor_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    
    tags$style(HTML("
      .pill-button.active {
        background-color: #00C1FF !important;
        color: white !important;
      }
      .pill-button:hover {
        opacity: 0.85;
      }
      
      .component-btn.active {
        background-color: #00C1FF !important;
        color: white !important;
      }
      .component-btn:hover {
        opacity: 0.85;
      }
    ")),
    tags$script(HTML("
      $(document).on('click', '.topic-page .pill-button', function(e) {
        var $container = $(this).closest('.topic-page');
        $container.find('.pill-button').removeClass('active');
        $(this).addClass('active');
      });
      $(document).on('click', '.component-btn', function(e) {
        $('.component-btn').removeClass('active');
        $(this).addClass('active');
      });
    ")),
    
    tags$div(class = "topic-page",
             
             # ============================================================
             # LAYOUT PRINCIPAL (2 COLUMNAS)
             # ============================================================
             fluidRow(
               
               # ----------------------------------------------------------
               # COLUMNA IZQUIERDA
               # ----------------------------------------------------------
               column(
                 width = 3,
                 class = "left-panel",
                 tags$div(class = "topic-header",
                          h1(class = "topic-title", "Non-salary Labor Costs"),
                          p(class = "topic-subtitle",
                            "Bonuses, social contributions, severance, and other employer-paid costs")
                 ),
                 
                 # -------- NOTES --------
                 tags$div(
                   style = "margin-bottom: 30px;",
                   h3("NOTES", style = "color: #1e3a5f; font-weight: bold; border-bottom: 2px solid #5fc4e3; padding-bottom: 10px;"),
                   tags$p(
                     style = "font-size: 14px; line-height: 1.6;",
                     "Data represents non-salary labor costs as multiples of monthly wages. Different countries have varying bonus structures."
                   ),
                   tags$ul(
                     style = "font-size: 13px; line-height: 1.5;",
                     tags$li("Bolivia shows different scenarios, A to E, based on wage levels and tenure"),
                     tags$li("Colombia and Mexico have different calculation methods, A and B"),
                     tags$li("Paraguay and Peru show variations across different benefit categories, A to F"),
                     tags$li("Empty cells indicate the bonus type does not apply to that country or category")
                   )
                 ),
                 
                 # -------- SOURCES --------
                 tags$div(
                   style = "margin-bottom: 30px;",
                   h3("SOURCES", style = "color: #1e3a5f; font-weight: bold; border-bottom: 2px solid #5fc4e3; padding-bottom: 10px;"),
                   tags$p(
                     style = "font-size: 13px;",
                     "TBD"
                   )
                 ),
                 
                 # -------- DETAILS --------
                 tags$div(
                   style = "margin-bottom: 30px;",
                   h3("DETAILS", style = "color: #1e3a5f; font-weight: bold; border-bottom: 2px solid #5fc4e3; padding-bottom: 10px;"),
                   tags$p(
                     style = "font-size: 13px;",
                     "TBD"
                   )
                 ),
                 
                 # -------- DOWNLOAD & SHARE BUTTONS --------
                 tags$div(
                   style = "display: flex; gap: 10px;",
                   # actionButton(
                   #   ns("btn_download"),
                   #   "DOWNLOAD",
                   #   style = "background-color: #1e3a5f; color: white; border-radius: 25px; padding: 10px 20px; font-weight: bold; border: none;"
                   # ),
                   downloadButton(
                     outputId = ns("download_df"),
                     label = "DOWNLOAD",
                     style = "background-color: #1e3a5f; color: white; border-radius: 25px; padding: 10px 20px; font-weight: bold; border: none;"
                     
                   ),
                   actionButton(
                     ns("btn_share"),
                     "SHARE",
                     style = "background-color: #1e3a5f; color: white; border-radius: 25px; padding: 10px 20px; font-weight: bold; border: none;"
                   )
                 )
               ),
               
               # ----------------------------------------------------------
               # COLUMNA DERECHA
               # ----------------------------------------------------------
               column(
                 width = 9,
                 class = "right-panel",
                 
                 # -------- FILTROS --------
                 fluidRow(
                   column(
                     width = 12,
                     
                     tags$div(
                       style = "display: flex; align-items: center; gap: 20px; margin-bottom: 20px;",
                       
                       
                       
                       # -------- Wage FILTER --------
                       tags$div(
                          style = "display: flex; flex-direction: column; gap: 10px;",
                          
                          # ----- Título -----
                          tags$div(
                            "Non-Salary Labor Costs as Multiples of Minimum Wages",
                            style = "font-weight: bold; color: #b0b0b0; font-size: 14px; margin-bottom: 5px;"
                          ),
                          
                          # ----- Fila de botones -----
                          tags$div(
                            style = "display: flex; gap: 8px;",
                            
                            actionButton(ns("btn_sm1"), "1MW",
                                        class = "pill-button active",
                                        style = "background-color: #f0f0f0; color: #333;
                                                  border: none; border-radius: 20px; padding: 6px 18px;"),
                            
                            actionButton(ns("btn_sm2"), "2MW",
                                        class = "pill-button",
                                        style = "background-color: #f0f0f0; color: #333;
                                                  border: none; border-radius: 20px; padding: 6px 18px;"),
                            
                            actionButton(ns("btn_sm5"), "5MW",
                                        class = "pill-button",
                                        style = "background-color: #f0f0f0; color: #333;
                                                  border: none; border-radius: 20px; padding: 6px 18px;"),
                            
                            actionButton(ns("btn_sm10"), "10MW",
                                        class = "pill-button",
                                        style = "background-color: #f0f0f0; color: #333;
                                                  border: none; border-radius: 20px; padding: 6px 18px;"),
                            
                            actionButton(ns("btn_sm15"), "15MW",
                                        class = "pill-button",
                                        style = "background-color: #f0f0f0; color: #333;
                                                  border: none; border-radius: 20px; padding: 6px 18px;")
                          ),
                          
                          # ----- Select debajo -----
                          tags$div(
                            style = "margin-top: 5px;",
                            uiOutput(ns("country_selection"))
                          )
                        ),

                       
                       # Separador
                       tags$div(style = "width: 2px; height: 30px; background-color: #e0e0e0; margin: 0 20px;"),
                       
                       
                       tags$div(style = "display: flex; flex-direction: column; gap: 6px; position: relative;",
                          tags$div(


                          style = "display: flex; gap: 8px; align-items: center;",
                          
                          # -------- VIEW FILTER --------
                          tags$span("Data breakdown by:", style = "font-weight: bold; color: #b0b0b0; font-size: 14px;"),
                       
                          actionButton(ns("btn_total"), "TOTAL",
                                        class = "pill-button active",
                                        style = "background-color: #f0f0f0; color: #333; border-radius: 20px; padding: 6px 18px;"),
                          
                          actionButton(ns("btn_payer"), "BY PAYER",
                                        class = "pill-button",
                                        style = "background-color: #f0f0f0; color: #333; border-radius: 20px; padding: 6px 18px;"),
                          
                          actionButton(ns("btn_component"), "BY COMPONENT",
                                        class = "pill-button",
                                        style = "background-color: #f0f0f0; color: #333; border-radius: 20px; padding: 6px 18px;")
                        ),
                        tags$div(
                            class = "component-wrapper-fixed",
                            uiOutput(ns("component_buttons"))
                        ),
                        tags$div(
                          class = "component-wrapper-fixed",
                          uiOutput(ns("bonus_buttons"))
                        )
                       )
                     )
                   )
                 ),
                 
                 tags$hr(style = "border-top: 2px solid #00b8d4;"),
                 tags$hr(),
                 
                 # -------- GRÁFICO --------
                 plotlyOutput(ns("plot"), height = "500px"),
                 div(
                   style = "margin-top:30px;",
                   reactable::reactableOutput(ns("tabla_detalle"))
                 )
               )
             )
    )
  )
}

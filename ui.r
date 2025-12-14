# ============================
# UI sources for each page
# ============================


source("tabs/ui/guide.R", local = TRUE)
source("tabs/ui/about.R", local = TRUE)
source("tabs/ui/forthcoming.R", local = TRUE) 

# ============================
# MAIN UI
# ============================

shinyUI(
  fluidPage(
    shinyjs::useShinyjs(),
    
    # ---- HEAD ----
    tags$head(
      tags$title("Regulatory Frameworks Explorer"),
      tags$link(
        href = "https://fonts.googleapis.com/css2?family=Source+Serif+Pro:wght@400;600&family=Source+Sans+Pro:wght@300;400;600&display=swap",
        rel = "stylesheet"
      ),
      includeCSS("www/styles.css"),
      
      # ---- JAVASCRIPT TO CONTROL TABS ----
      tags$script(HTML("
        Shiny.addCustomMessageHandler('trigger-download', function(id) {
          const el = document.getElementById(id);
          if (el) el.click();
        });
      ")),
      tags$script(HTML("
      document.addEventListener('DOMContentLoaded', function() {
    
        // Function to update active state
        function updateActiveNav(activeTab) {
          // Remove active class from all nav links
          document.querySelectorAll('.nav-link').forEach(function(link) {
            link.classList.remove('active');
          });
          
          // Add active class to the current tab
          const activeLink = document.querySelector('.nav-link[data-tab=\"' + activeTab + '\"]');
          if (activeLink) {
            activeLink.classList.add('active');
          }
        }
    
        // Set initial active state (landing page)
        updateActiveNav('landing');
    
        // Attach click handler to each header nav link
        document.querySelectorAll('.nav-link').forEach(function(link) {
          link.addEventListener('click', function(e) {
            e.preventDefault();
    
            // Get the tab name from data-tab attribute
            let tab = this.getAttribute('data-tab');
    
            // Update active state immediately
            updateActiveNav(tab);
    
            // Find the hidden Shiny tab button that matches this name
            let tabButton = document.querySelector(
              'a[data-value=\"' + tab + '\"]'
            );
    
            // Simulate a click to switch the tab
            if (tabButton) {
              tabButton.click();
            }
          });
        });
    
        // Listen for tab changes (in case tabs are changed programmatically)
        const observer = new MutationObserver(function(mutations) {
          mutations.forEach(function(mutation) {
            if (mutation.attributeName === 'class') {
              const tabs = document.querySelectorAll('#main_tabs > .tab-pane');
              tabs.forEach(function(tab) {
                if (tab.classList.contains('active')) {
                  const tabValue = tab.getAttribute('data-value');
                  if (tabValue) {
                    updateActiveNav(tabValue);
                  }
                }
              });
            }
          });
        });
    
        // Observe tab changes
        const tabPanes = document.querySelectorAll('#main_tabs > .tab-pane');
        tabPanes.forEach(function(pane) {
          observer.observe(pane, { attributes: true });
        });
    
      });
    "))
    ),
    
    # ---- HEADER ----
    tags$div(
      class = "header",
      tags$div(
        class = "header-content",    # ← Cambio de clase
        
        # Logo PRIMERO (izquierda)
        tags$img(src = "WB.png", class = "wb-logo"),
        
        # Menú DESPUÉS (derecha)
        tags$div(
          class = "nav-menu",
          tags$a("Explorer", class = "nav-link", `data-tab` = "landing"),
          tags$a("Guide", class = "nav-link", `data-tab` = "Guide"),
          tags$a("About", class = "nav-link", `data-tab` = "About")
        )
      )
    ),
    
    # ---- MAIN BODY ----
    div(
      class = "main-content",
      tabsetPanel(
        id = "main_tabs",
        type="hidden",
        selected = "landing",
        
        # ============================
        # 1. LANDING PAGE
        # ============================
        tabPanel(
          "landing",
          tags$section(
            class = "page-section",
            tags$div(
              class = "container",
              
              # Hero Banner
              tags$div(class = "hero-banner"),
              # Two Column Layout for Title and Description
              tags$div(
                class = "row",
                style = "margin-bottom: 60px;",
                
                # Left Column: Title
                tags$div(
                  class = "col-md-6",
                  h1(
                    class = "page-title",
                    style = "border-bottom: 3px solid #00C1FF; padding-bottom: 15px; display: inline-block;",
                    "Regulatory", tags$br(), "Frameworks Explorer"
                  )
                ),
                
                # Right Column: Description
                tags$div(
                  class = "col-md-6",
                  p(
                    class = "page-subtitle",
                    style = "margin-top: 0;",
                    "Explore comprehensive data on non-salary labor costs, minimum wages, and business taxes, across  countries. Dive into interactive visualizations and detailed analyses to understand regional regulatory frameworks."
                  )
                )
              ),
              
              # Section Divider
              tags$div(
                class = "section-divider",
                "CHOOSE A REGULATORY FRAMEWORK TOPIC TO EXPLORE DETAILED DATA AND VISUALIZATIONS"
              ),
              
              
              tags$div(class = "topic-grid",
                       tags$div(class = "topic-card", 
                                onclick = "Shiny.setInputValue('topic_selected', 'labor', {priority: 'event'})",
                                h3("Non-salary Labor Costs"),
                                p("Yearly bonuses, social security contributions, and employment benefits")
                       ),
                       # Minimum Wages
                       tags$div(
                         class = "topic-card disabled",
                         onclick = "document.querySelector('a[data-value=\"forthcoming\"]').click();",
                         h3("Minimum Wages"),
                         p("Minimum wage policies and trends across the region"),
                         tags$span(class = "topic-card-badge", "FORTHCOMING")
                       ),
                       
                       # Business Taxes
                       tags$div(
                         class = "topic-card disabled",
                         onclick = "document.querySelector('a[data-value=\"forthcoming\"]').click();",
                         h3("Business Taxes"),
                         p("Corporate tax rates, incentives, and fiscal policies"),
                         tags$span(class = "topic-card-badge", "FORTHCOMING")
                       )
                       
                       
                                       
              ),
            
              tags$div(
                class = "footer",
                tags$p(class = "footer-text", "© 2025 World Bank Group")
              )     
            )
          )
        ),
        
        # ============================
        # 2. Guide 
        # ============================
        tabPanel("Guide", guide), 
        
        # ============================
        # 3. About
        # ============================
        tabPanel(
          "About", about
        ),
        # En la sección de tabsetPanel, después de "About"
        tabPanel(
          "forthcoming",
          forthcoming
        ),
        # ============================
        # 4. CONTENT MODULE PAGE
        # ============================
        tabPanel(
          "content",
          div(
            class = "content-area",
            uiOutput("dynamic_content")
          )
        )
      )
    )
  )
)


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
    ")),
      tags$script(HTML("
      document.addEventListener('DOMContentLoaded', function() {
        const TAB_PARAM = 'tab';
        const tabContainer = document.getElementById('main_tabs');
        let suppressPush = false;
        let currentTab = null;

        function getActiveTab() {
          if (!tabContainer) return null;
          const active = tabContainer.querySelector('.tab-pane.active');
          return active ? active.getAttribute('data-value') : null;
        }

        function syncUrl(tab, replace) {
          if (!tab) return;
          const url = new URL(window.location);
          url.searchParams.set(TAB_PARAM, tab);
          const method = replace ? 'replaceState' : 'pushState';
          window.history[method]({ tab: tab }, '', url);
        }

        function switchToTab(tab, fromPop) {
          if (!tab) return;
          if (fromPop) suppressPush = true;
          const btn = document.querySelector('a[data-value=\"' + tab + '\"]');
          if (btn) btn.click();
        }

        // Hook Bootstrap tab events (Shiny uses BS tabs under the hood)
        if (window.jQuery) {
          window.jQuery(document).on('shown.bs.tab', 'a[data-toggle=\"tab\"]', function(e) {
            const tab = window.jQuery(e.target).data('value');
            currentTab = tab || currentTab;
            if (!suppressPush && tab) syncUrl(tab, false);
            suppressPush = false;
          });
        }

        if (tabContainer) {
          const observer = new MutationObserver(function() {
            const tab = getActiveTab();
            if (!tab) return;
            if (suppressPush) {
              suppressPush = false;
              syncUrl(tab, true);
              return;
            }
            syncUrl(tab, false);
            currentTab = tab;
          });
          tabContainer.querySelectorAll('.tab-pane').forEach(function(pane) {
            observer.observe(pane, { attributes: true, attributeFilter: ['class'] });
          });
        }

        const initialTab = new URL(window.location).searchParams.get(TAB_PARAM) || getActiveTab() || 'landing';
        suppressPush = true;
        switchToTab(initialTab, true);
        syncUrl(initialTab, true);
        currentTab = initialTab;

        window.addEventListener('popstate', function(event) {
          const tabFromState = event.state && event.state.tab;
          const tabFromUrl = new URL(window.location).searchParams.get(TAB_PARAM);
          switchToTab(tabFromState || tabFromUrl, true);
        });
      });
    "))
    ),
    tags$script(HTML("
  document.addEventListener('DOMContentLoaded', function () {
    const btn = document.querySelector('.hamburger-btn');
    const menu = document.querySelector('.hamburger-dropdown');

    btn.addEventListener('click', function (e) {
      e.stopPropagation();
      menu.classList.toggle('hidden');
    });

    document.addEventListener('click', function () {
      menu.classList.add('hidden');
    });
  });
")),
    

    # ---- HEADER ----
    tags$div(
      class = "header",
      tags$div(
        class = "header-content",
        
        # Logo (izquierda)
        tags$img(src = "WB.png", class = "wb-logo"),
        
        # Spacer (centro) — mantiene alineación del grid
        tags$div(),
        
        # Hamburger (derecha)
        tags$div(
          class = "hamburger-menu",
          
          # Botón ☰
          tags$div(
            class = "hamburger-btn",
            HTML("&#9776;")  # ☰
          ),
          
          # Dropdown
          tags$div(
            class = "hamburger-dropdown hidden",
            
            tags$a(
              "Home",
              class = "nav-link",
              onclick = "document.querySelector('a[data-value=\"landing\"]').click();"
            ),
           
            
            tags$hr(),
            
            tags$a(
              "Non-Salary Labor Costs",
              class = "nav-link",
              onclick = "Shiny.setInputValue('topic_selected', 'labor', {priority: 'event'})"
            ),
            tags$a(
              "Minimum Wages",
              class = "nav-link",
              onclick = "document.querySelector('a[data-value=\"forthcoming\"]').click();"
            ),
            tags$a(
              "Business Taxes",
              class = "nav-link",
              onclick = "document.querySelector('a[data-value=\"forthcoming\"]').click();"
            ),
            tags$hr(),
            tags$a(
              "Guide",
              class = "nav-link",
              onclick = "document.querySelector('a[data-value=\"Guide\"]').click();"
            ),
            tags$a(
              "About",
              class = "nav-link",
              onclick = "document.querySelector('a[data-value=\"About\"]').click();"
            ),
            
          )
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
              class = "landing-container",
              
              # Hero Banner
              #tags$div(class = "hero-banner"),
              
              # Two Column Layout for Title and Description
              tags$div(
                class = "row",
                style = "margin-bottom: 60px;",
                
                # Left Column: Title
                tags$div(
                  class = "col-md-6",
                  h1(
                    class = "page-title",
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
              
              tags$div(
                class = "nav-menu-center",
                tags$a(
                  class = "nav-link nav-card",
                  `data-tab` = "Guide",
                  tags$span(class = "nav-link-circle", style = "background-image: url('guide-circle.jpg');"),
                  tags$span(class = "nav-link-title", "Guide"),
                  tags$span(class = "nav-link-desc", "Learn how to read and navigate the data.")
                ),
                tags$a(
                  class = "nav-link nav-card",
                  `data-tab` = "About",
                  tags$span(class = "nav-link-circle", style = "background-image: url('about-circle.jpg');"),
                  tags$span(class = "nav-link-title", "About"),
                  tags$span(class = "nav-link-desc", "Meet the team and project goals.")
                )
              ),
              
              # Section Divider
              tags$div(
                class = "section-divider",
                "Choose a Topic To Explore"
              ),
              
              
              tags$div(class = "topic-grid",
                       tags$div(
                         class = "topic-card", 
                         onclick = "Shiny.setInputValue('topic_selected', 'labor', {priority: 'event'})",
                         tags$div(class = "topic-card-image labor-img", style = "background-image: url('topic-labor.png');"),
                         tags$div(
                           class = "topic-card-body",
                           h3(class = "topic-card-title", "Non-Salary Labor Costs"),
                           p(class = "topic-card-description", "Yearly bonuses, social security contributions, and employment benefits")
                         )
                       ),
                       # Minimum Wages
                       tags$div(
                         class = "topic-card disabled",
                         onclick = "document.querySelector('a[data-value=\"forthcoming\"]').click();",
                         tags$div(class = "topic-card-image minwage-img"),
                         tags$div(
                           class = "topic-card-body",
                           h3(class = "topic-card-title", "Minimum Wages"),
                           p(class = "topic-card-description", "Minimum wage policies and trends across the region"),
                           tags$span(class = "topic-card-badge", "FORTHCOMING")
                         )
                       ),
                       
                       # Business Taxes
                       tags$div(
                         class = "topic-card disabled",
                         onclick = "document.querySelector('a[data-value=\"forthcoming\"]').click();",
                         tags$div(class = "topic-card-image btax-img"),
                         tags$div(
                           class = "topic-card-body",
                           h3(class = "topic-card-title", "Business Taxes"),
                           p(class = "topic-card-description", "Corporate tax rates, incentives, and fiscal policies"),
                           tags$span(class = "topic-card-badge", "FORTHCOMING")
                         )
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

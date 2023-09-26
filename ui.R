# Define UI ----
source('Tabs.R')
ui <- fluidPage(theme = shinytheme("lumen"),
                shinyjs::useShinyjs(),
                titlePanel("Adaptive Learning - HL"),
                tabsetPanel(id = "tabs" ,type = "tabs",
                            MainTab,
                            SummaryTab,
                            FreqTab,
                            GraphTab
                )
)

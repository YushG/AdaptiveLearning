# Main Tab ----
MainTab <- tabPanel("Main", 
         sidebarLayout(
           sidebarPanel(width=12,
                        fileInput("file1", "Choose CSV File", accept = ".csv"),
                        fluidRow(
                          column(width = 2, 
                                 textInput("premodText", "Pre-modifier:", placeholder = "Enter pre-modifier then hit submit")
                          ),
                          column(width = 2, 
                                 textInput("preText", "Pre:", placeholder = "Enter pre then hit submit")
                          ),
                          column(width = 2, 
                                 textInput("aspectText", "Aspect:", placeholder = "Enter aspect then hit submit")
                          ),
                          column(width = 2, 
                                 textInput("postText", "Post:", placeholder = "Enter post then hit submit")
                          ),
                          column(width = 2, 
                                 textInput("postmodText", "Post-modifier:", placeholder = "Enter post-modifier then hit submit")
                          ),
                          column(width = 2, 
                                 textInput("chrlmt", "CHR Limit:", placeholder = "Enter number to limit chars for value")
                          )
                        ),
                        fluidRow(
                          column(width = 2,
                                 textInput("excludePre", "Exclude:", placeholder = "Exclude pre-modifier then hit submit")
                          ),
                          column(width = 1,
                                 actionButton("sgt1", "Suggest", disabled=TRUE)
                          ),
                          column(width = 1,
                                 selectInput("no1", label=NULL, c("1", "2", "3", "4", "5"))
                          ),
                          column(width = 2,
                                 actionButton("sgt3", "Suggest", disabled=TRUE)
                          ),
                          column(width = 1,
                                 actionButton("sgt2", "Suggest", disabled=TRUE)
                          ),
                          column(width = 1,
                                 selectInput("no2", label=NULL, c("1", "2", "3", "4", "5"))
                          ),
                          column(width = 2,
                                 textInput("excludePost", "Exclude:", placeholder = "Exclude post-modifier then hit submit")
                          )
                        ),
                        br(),
                        fluidRow(
                          column(width = 2,
                                 actionButton("view", "View", disabled=TRUE)
                          ),
                          column(width = 10,
                                 switchInput(inputId="filter", value = FALSE, onLabel="Unlabeled", offLabel="All")
                          )
                        ),
                        fluidRow(
                          column(width = 12,
                                 actionButton("pos", "Positive", class = "btn-danger", disabled=TRUE),
                                 actionButton("neg", "Negative", class = "btn-success", disabled=TRUE),
                                 actionButton("ig", "Ignore", class = "btn-primary", disabled=TRUE),
                                 actionButton("clear", "Clear", class = "btn-primary", disabled=TRUE),
                                 actionButton("question", "Questionable", class="btn-warning", disabled=TRUE)
                          )
                        ),
                        br(),
                        fluidRow(
                          column(width = 12,
                                 actionButton("save", "Save", class = "btn-info", disabled=TRUE)
                          )
                        )
           ),
           mainPanel(
             fluidRow(
               column(width = 6,
                      textOutput('numberRows')
               ),
               column(width = 6,
                      progressBar(id = "percentageLabeled", title="Percent Labeled:", display_pct = TRUE, value=0)
               )
             ),
             textOutput('dataTableRows'),
             dataTableOutput('contents'),
             width = 12
           )
         )
)

# Summary Tab ----
SummaryTab <- tabPanel("User Summary",
         mainPanel(
           fluidRow(
             column(12,
                    dataTableOutput("summary")
             )
           ),
           hr(),
           fluidRow(
             column(width = 8,
                    fileInput("load_summary", "Load Summary (CSV File)",
                              accept = c(
                                "text/csv",
                                "text/comma-separated-values,text/plain",
                                ".csv")
                    )
             ),
             column(width = 4,
                    actionButton("save_summary", "Save", class = "btn-info")
             )
           )
         )
)

# Frequency Tab ----
FreqTab <- tabPanel("Aspect Frequency",
         mainPanel(
           dataTableOutput("analytics")
         )
)

# Graph Tab ----
GraphTab <- tabPanel("Graphs",
         mainPanel(
           column(width = 6,
                  plotOutput("pa_main", click = "plot_click"),
                  plotOutput("pa_prime_main", click = "plot_click")
           ),
           column(width = 6,
                  plotOutput("sa_main", click = "plot_click"),
                  plotOutput("sa_prime_main", click = "plot_click")
           )
         )
)

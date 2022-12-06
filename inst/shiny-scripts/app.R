# The codes are adapted and followed the package MPLNClust.
# Reference: Silva, Anjali, et al.
# "A multivariate Poisson-log normal mixture model for clustering transcriptome
# sequencing data." BMC bioinformatics 20.1 (2019): 1-11.

library(shiny)
library(shinyalert)

# Define The UI for the app ----
ui <- fluidPage(

  # App title ----
  titlePanel(tags$h1(tags$b("oncoAnalysis:"),"Check Base Change Infomation")),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      tags$p("Description: This is a Shiny App for oncoAnalysis R package
             (Xu Xinyi et al., 2022). The oncoAnalysis is an R package check if
             there is mutations between healthy input DNA FASTA file and
             mutated(suspected) DNA FASTA file. The current version contains
             one FASTA reading function, two analysis functions and two plotting
             functions. The FASTA reading function was able to read in FATSA
             files, check if the DNA sequence is valid and output sequence.
             The two analysis functions are able to compare the contents of
             two DNA FASTA files and obtain the mutation details which current
             work seem to be lack of. The plotting functions visualize the
             results obtained from the analysis functions and display them in
             a clearer way."),

      # br() element to introduce extra vertical spacing ----
      br(),
      br(),

      # input
      tags$b("Instructions: Below choose two sample datasets.
             Navigate through the different tabs to the right to explore the results."),

      # br() element to introduce extra vertical spacing ----
      br(),
      br(),
      br(),
      # input
      # shinyalert::useShinyalert(),  # Comment out the useShinyalert
      uiOutput("tab2"),
      actionButton(inputId = "Sample_Healthy",
                   label = "Sample Data Details"),
      uiOutput("tab1"),
      actionButton(inputId = "Sample_Mutated",
                   label = "Sample Data Details"),
      fileInput(inputId = "file1",
                label = "Dataset: Select a FASTA dataset to analyze. File should be
                in .fasta format with DNA sequence contained and suspected healthy.
                You may download an example dataset above and explore first.",
                accept = c(".fasta")),
      fileInput(inputId = "file2",
                label = "Dataset: Select a FASTA dataset to analyze. File should be
                in .fasta format with DNA sequence contained and suspected mutated.
                You may download an example dataset above and explore first.",
                accept = c(".fasta")),

      # br() element to introduce extra vertical spacing ----
      br(),

      # ActionButton
      actionButton(inputId = "button2",
                   label = "Run"),

      # br() element to introduce extra vertical spacing -
      br(),

    ), # End of side pannel


    # Main panel for displaying outputs
    mainPanel(

      # Output: Tabet
      tabsetPanel(type = "tabs",
                  tabPanel("DNA Sequence",
                           h3("Instructions: Click 'Run' at the bottom left side."),
                           h3("The DNA sequence contained in the FASTA file:"),
                           br(),
                           verbatimTextOutput("textOut")),
                  tabPanel("Base Change Details",
                           h3("Instructions: Click 'Run' at the bottom left side."),
                           h3("The Base Change Details: "),
                           h5("Note: If the length of the healthy and mutated sequences
                              are different, the Base Change Table, Mutation Plot,
                              and Mutation Comparison Plot section would not output
                              information for the current version. The Base Change Details
                              section would output if there is deletion or insertion
                              happened."),
                           br(),
                           verbatimTextOutput("mutChecker")),
                  tabPanel("Base Change Table",
                           h3("Instructions: Click 'Run' at the bottom left side."),
                           h3("The Base Change Table:"),
                           h4("Var1 represents Healthy version."),
                           h4("Var2 represents Mutated version."),
                           br(),
                           tableOutput("mutTable")),
                  tabPanel("Mutation Plot",
                           h3("Instructions: Click 'Run' at the bottom left side."),
                           h3("The Mutation Plot:"),
                           br(),
                           plotOutput('mutPlot')),
                  tabPanel("Mutation Comparison Plot",
                           h3("Instructions: Click 'Run' at the bottom left side."),
                           h3("Mutation Comparison Plot:"),
                           br(),
                           plotOutput('mutCompPlot'))

      )
    )
  )
)

# Define server logic for random distribution app ----
server <- function(input, output) {

  # Reactive expression to generate the requested distribution ----
  # This is called whenever the inputs change. The output functions
  # defined below then use the value computed from this expression


  # Step I: Save input fasta as a reactive
  fileinput_healthy <- reactive({
    if (! is.null(input$file1)) {
      oncoAnalysis::fastaReader(input$file1$datapath)
    }
  })

  fileinput_mutated <- reactive({
    if (! is.null(input$file2)) {
      oncoAnalysis::fastaReader(input$file2$datapath)
    }
  })

  startprinting <- eventReactive(eventExpr = input$button2, {
    withProgress(message = 'DNA Sequence Printing', value = 0, {
      # Check if file is NULL, if not print sequence.

      if (length(fileinput_healthy) == length(fileinput_mutated)) {
        mutCheckerResults <- oncoAnalysis::mutChecker(fileinput_healthy,
                                                      fileinput_mutated)
      }
    })
  })

  # Textoutput
  output$textOut <- renderPrint({
    # Get the healthy sequence
    if (! is.null(input$file1)) {
      print("The Healthy(control) DNA sequence: ")
      print(oncoAnalysis::fastaReader(input$file1$datapath))
    }

    # Get the suspected mutated sequence
    if (! is.null(input$file2)) {
      print("The Suspected Mutated DNA sequence: ")
      print(oncoAnalysis::fastaReader(input$file2$datapath))
    }
  })

  # Step II:
  # MutChecker
  output$mutChecker <- renderPrint({
    # Get the healthy sequence
    if (! is.null(input$file1)) {
      healthyseq <- oncoAnalysis::fastaReader(input$file1$datapath)
    }

    # Get the suspected mutated sequence
    if (! is.null(input$file2)) {
      mutatedseq <- oncoAnalysis::fastaReader(input$file2$datapath)
    }

    # Check for Mutation Results, if length is the same output info, otherwise
    # Output if deletion or insertion happened.
    if (length(healthyseq) == length(mutatedseq)) {
      mutCheckerResults <- oncoAnalysis::mutChecker(healthyseq, mutatedseq)

      mutCheckerResults
    } else if (length(healthyseq) != length(mutatedseq)) {
      mutCheckerResults <- oncoAnalysis::mutChecker(healthyseq, mutatedseq)
    }
  })

  # MutTable
  output$mutTable <- renderTable({
    # Get the healthy sequence
    if (! is.null(input$file1)) {
      healthyseq <- oncoAnalysis::fastaReader(input$file1$datapath)
    }

    # Get the suspected mutated sequence
    if (! is.null(input$file2)) {
      mutatedseq <- oncoAnalysis::fastaReader(input$file2$datapath)
    }

    # If the length of sequence is the same, output the mutation table.
    if (length(healthyseq) == length(mutatedseq)) {
      mutCheckerResults <- oncoAnalysis::mutChecker(healthyseq, mutatedseq)

      mutation_table <- oncoAnalysis::mutTable(mutCheckerResults$MutMatrix)

      mutation_table
    }
  })


  # Step III: Plotting

  # plot mutPlot
  output$mutPlot <- renderPlot({
    # Get the healthy sequence
    if (! is.null(input$file1)) {
      healthyseq <- oncoAnalysis::fastaReader(input$file1$datapath)
    }

    # Get the suspected mutated sequence
    if (! is.null(input$file2)) {
      mutatedseq <- oncoAnalysis::fastaReader(input$file2$datapath)
    }

    # If the length of sequence is the same, plot mutPlot.
    if (length(healthyseq) == length(mutatedseq)) {
      mutCheckerResults <- oncoAnalysis::mutChecker(healthyseq, mutatedseq)

      oncoAnalysis::mutPlot(mutvals = mutCheckerResults)
    }
  })

  # plot mutCompPlot
  output$mutCompPlot <- renderPlot({
    # Get the healthy sequence
    if (! is.null(input$file1)) {
      healthyseq <- oncoAnalysis::fastaReader(input$file1$datapath)
    }

    # Get the suspected mutated sequence
    if (! is.null(input$file2)) {
      mutatedseq <- oncoAnalysis::fastaReader(input$file2$datapath)
    }

    # Plot the Mutation Comparison Plot if the length of the sequence
    # is the same.
    if (length(healthyseq) == length(mutatedseq)) {
      oncoAnalysis::mutCompPlot(healthyseq, mutatedseq)
    }
  })

  # URLs for downloading data
  url1 <- a("Example Dataset Mutated", href="https://github.com/xuxiny17/oncoAnalysis/blob/main/inst/extdata/samplemut.fasta")
  output$tab1 <- renderUI({
    tagList("Download:", url1)
  })

  observeEvent(input$Sample_Mutated, {
    # Show a modal when the button is pressed
    shinyalert(title = "Sample Data Set(Mutated)",
               text = "This is a Data set containing Mutated Sample DNA Sequences(P53).
               Data was self generated in November, 2022 using write.fasta() function in SeqinR R package.
               To save the file, click on link, then click 'Download' from the top right side.
               Citation: Xu Xinyi (2022). Sequnce obtained from NCBI. URL https://www.ncbi.nlm.nih.gov/",
               type = "info")
  })

  url2 <- a("Example Dataset Healthy(Control)", href="https://github.com/xuxiny17/oncoAnalysis/blob/main/inst/extdata/sample.fasta")
  output$tab2 <- renderUI({
    tagList("Download:", url2)
  })

  observeEvent(input$Sample_Healthy, {
    # Show a modal when the button is pressed
    shinyalert(title = "Sample Data Set(Healthy)",
               text = "This is a Data set containing Sample DNA Sequences (Healthy).
               Data was self generated in November, 2022 using write.fasta() function in SeqinR R package.
               To save the file, click on link, then click 'Download' from the top right side.
               Citation: Xu Xinyi (2022). Sequnce obtained from NCBI. URL https://www.ncbi.nlm.nih.gov/",
               type = "info")
  })


}

# Create the Shiny app ----
shinyApp(ui, server)

# [END]

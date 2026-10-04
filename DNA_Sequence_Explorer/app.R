# ============================================================
# 🧬 INTERACTIVE DNA SEQUENCE ANALYSIS & MUTATION EXPLORER
# ============================================================
# STEP-BY-STEP VERSION
#
# Page 1  -> Welcome
# Page 2  -> DNA Input
# Page 3  -> Sequence Analysis
# Page 4  -> Pattern & Codon Analysis
# Page 5  -> Reading Frames
# Page 6  -> Mutation Explorer
# Page 7  -> Final Summary
# ============================================================


# ============================================================
# LIBRARIES
# ============================================================

library(shiny)
library(ggplot2)
library(DT)


# ============================================================
# USER INTERFACE
# ============================================================

ui <- fluidPage(
  
  # ==========================================================
  # CUSTOM CSS
  # ==========================================================
  
  tags$head(
    
    tags$style(HTML("

      body {
        background-color: #f4f7fb;
        font-family: Arial, sans-serif;
      }

      /* ---------------- HEADER ---------------- */

      .app-title {
        background: linear-gradient(135deg, #12372A, #436850);
        color: white;
        padding: 25px;
        border-radius: 12px;
        margin-bottom: 20px;
        text-align: center;
      }

      .app-title h1 {
        font-size: 32px;
        font-weight: bold;
        margin-bottom: 10px;
      }

      .app-title p {
        font-size: 16px;
        margin-bottom: 5px;
      }

      /* ---------------- CARDS ---------------- */

      .card {
        background-color: white;
        padding: 25px;
        border-radius: 12px;
        margin-bottom: 20px;
        box-shadow: 0px 3px 10px rgba(0,0,0,0.08);
      }

      .section-title {
        color: #12372A;
        font-weight: bold;
        margin-bottom: 15px;
      }

      /* ---------------- STAT BOX ---------------- */

      .stat-box {
        background-color: white;
        border-radius: 12px;
        padding: 20px;
        text-align: center;
        box-shadow: 0px 3px 10px rgba(0,0,0,0.08);
        margin-bottom: 15px;
      }

      .stat-number {
        font-size: 28px;
        font-weight: bold;
        color: #12372A;
      }

      .stat-label {
        color: #666666;
        font-size: 14px;
      }

      /* ---------------- BUTTONS ---------------- */

      .next-button {
        background-color: #12372A;
        color: white;
        font-size: 16px;
        font-weight: bold;
        border-radius: 8px;
        padding: 10px 25px;
        border: none;
      }

      .next-button:hover {
        background-color: #436850;
        color: white;
      }

      .back-button {
        background-color: #777777;
        color: white;
        font-size: 16px;
        font-weight: bold;
        border-radius: 8px;
        padding: 10px 25px;
        border: none;
      }

      .back-button:hover {
        background-color: #555555;
        color: white;
      }

      .analyze-button {
        width: 100%;
        background-color: #12372A;
        color: white;
        font-size: 17px;
        font-weight: bold;
        border-radius: 8px;
        padding: 12px;
        border: none;
      }

      .analyze-button:hover {
        background-color: #436850;
        color: white;
      }

      .search-button {
        background-color: #436850;
        color: white;
        font-weight: bold;
        border-radius: 7px;
        padding: 8px 18px;
        border: none;
      }

      /* ---------------- NOTICE ---------------- */

      .notice {
        background-color: #fff8e1;
        border-left: 5px solid #f0ad4e;
        padding: 15px;
        margin-top: 15px;
        border-radius: 6px;
        font-size: 13px;
      }

      .info-box {
        background-color: #eef7f1;
        border-left: 5px solid #436850;
        padding: 15px;
        border-radius: 6px;
        margin-top: 10px;
      }

      /* ---------------- WELCOME ---------------- */

      .welcome-box {
        text-align: center;
        padding: 40px 20px;
      }

      .welcome-icon {
        font-size: 70px;
      }

      .welcome-title {
        color: #12372A;
        font-size: 34px;
        font-weight: bold;
        margin-top: 15px;
      }

      .welcome-subtitle {
        color: #555555;
        font-size: 18px;
        margin-bottom: 25px;
      }

      /* ---------------- STEP INDICATOR ---------------- */

      .step-indicator {
        background-color: white;
        padding: 15px;
        border-radius: 10px;
        margin-bottom: 20px;
        text-align: center;
        box-shadow: 0px 2px 8px rgba(0,0,0,0.06);
      }

      .step-text {
        color: #12372A;
        font-weight: bold;
        font-size: 15px;
      }

      /* ---------------- SEQUENCE DISPLAY ---------------- */

      .sequence-display {
        background-color: #f1f5f3;
        border-radius: 8px;
        padding: 15px;
        font-family: monospace;
        word-break: break-all;
        font-size: 16px;
      }

      /* ---------------- MUTATION ---------------- */

      .mutation-box {
        background-color: #fff8e1;
        border-left: 5px solid #f0ad4e;
        padding: 20px;
        border-radius: 8px;
        margin-top: 15px;
      }

    "))
    
  ),
  
  
  # ==========================================================
  # APPLICATION HEADER
  # ==========================================================
  
  div(
    class = "app-title",
    
    h1(
      "🧬 Interactive DNA Sequence Analysis"
    ),
    
    p(
      "DNA Sequence Pattern Analyzer & Mutation Explorer"
    ),
    
    p(
      "Educational Bioinformatics Platform"
    )
  ),
  
  
  # ==========================================================
  # STEP INDICATOR
  # ==========================================================
  
  uiOutput(
    "step_indicator"
  ),
  
  
  # ==========================================================
  # PAGE 1 - WELCOME
  # ==========================================================
  
  conditionalPanel(
    
    condition = "output.current_page == 1",
    
    div(
      class = "card welcome-box",
      
      div(
        class = "welcome-icon",
        "🧬"
      ),
      
      div(
        class = "welcome-title",
        "Welcome to DNA Sequence Explorer"
      ),
      
      div(
        class = "welcome-subtitle",
        "Interactive DNA Sequence Analysis & Mutation Explorer"
      ),
      
      p(
        "This application provides an educational environment for exploring DNA sequences using computational bioinformatics techniques."
      ),
      
      br(),
      
      div(
        class = "info-box",
        
        strong(
          "What you can explore"
        ),
        
        br(),
        br(),
        
        "🧬 DNA sequence validation",
        br(),
        
        "📊 Nucleotide composition",
        br(),
        
        "📈 GC and AT content",
        br(),
        
        "🔎 DNA pattern searching",
        br(),
        
        "🧪 Codon analysis",
        br(),
        
        "📖 Reading frames",
        br(),
        
        "🧬 Mutation comparison"
      ),
      
      br(),
      
      actionButton(
        "next1",
        "Start DNA Analysis →",
        class = "next-button"
      ),
      
      br(),
      br(),
      
      div(
        class = "notice",
        
        strong(
          "Privacy & Authorization Notice"
        ),
        
        br(),
        
        "Please analyze only DNA sequences that you own or are authorized to use. Do not upload another person's genetic information without appropriate permission and authorization."
      ),
      
      br(),
      
      div(
        class = "info-box",
        
        strong(
          "Educational Purpose"
        ),
        
        br(),
        
        "This application is intended for educational and research-support sequence exploration. It is not a clinical diagnostic system."
      )
    )
  ),
  
  
  # ==========================================================
  # PAGE 2 - DNA INPUT
  # ==========================================================
  
  conditionalPanel(
    
    condition = "output.current_page == 2",
    
    div(
      class = "card",
      
      h2(
        class = "section-title",
        "🧬 Step 1 — DNA Sequence Input"
      ),
      
      p(
        "Enter a DNA sequence manually or upload a FASTA file."
      ),
      
      textAreaInput(
        
        "dna_sequence",
        
        "Enter DNA sequence:",
        
        value = "ATGCGTACGTTAGCTAGCTAGGCTA",
        
        rows = 8,
        
        placeholder = "Example: ATGCGTACGTTAGC"
      ),
      
      fileInput(
        
        "fasta_file",
        
        "Or upload FASTA file:",
        
        accept = c(
          ".fasta",
          ".fa",
          ".txt"
        )
      ),
      
      actionButton(
        
        "analyze",
        
        "🔬 Validate & Analyze DNA",
        
        class = "analyze-button"
      ),
      
      br(),
      br(),
      
      uiOutput(
        "validation_message"
      ),
      
      div(
        class = "notice",
        
        strong(
          "Important:"
        ),
        
        br(),
        
        "Only DNA sequences containing A, T, G and C are accepted."
      )
    ),
    
    fluidRow(
      
      column(
        6,
        
        actionButton(
          "back2",
          "← Back",
          class = "back-button"
        )
      ),
      
      column(
        6,
        
        div(
          style = "text-align:right;",
          
          actionButton(
            "next2",
            "Next →",
            class = "next-button"
          )
        )
      )
    )
  ),
  
  
  # ==========================================================
  # PAGE 3 - SEQUENCE ANALYSIS
  # ==========================================================
  
  conditionalPanel(
    
    condition = "output.current_page == 3",
    
    div(
      class = "card",
      
      h2(
        class = "section-title",
        "📊 Step 2 — Sequence Analysis"
      ),
      
      fluidRow(
        
        column(
          4,
          
          div(
            class = "stat-box",
            
            div(
              class = "stat-number",
              
              textOutput(
                "sequence_length"
              )
            ),
            
            div(
              class = "stat-label",
              
              "Sequence Length"
            )
          )
        ),
        
        column(
          4,
          
          div(
            class = "stat-box",
            
            div(
              class = "stat-number",
              
              textOutput(
                "gc_content"
              )
            ),
            
            div(
              class = "stat-label",
              
              "GC Content"
            )
          )
        ),
        
        column(
          4,
          
          div(
            class = "stat-box",
            
            div(
              class = "stat-number",
              
              textOutput(
                "at_content"
              )
            ),
            
            div(
              class = "stat-label",
              
              "AT Content"
            )
          )
        )
      ),
      
      h4(
        "Validated DNA Sequence"
      ),
      
      div(
        class = "sequence-display",
        
        verbatimTextOutput(
          "validated_sequence"
        )
      )
    ),
    
    fluidRow(
      
      column(
        6,
        
        div(
          class = "card",
          
          h3(
            class = "section-title",
            "🧪 Nucleotide Composition"
          ),
          
          DT::DTOutput(
            "base_table"
          )
        )
      ),
      
      column(
        6,
        
        div(
          class = "card",
          
          h3(
            class = "section-title",
            "📈 Nucleotide Distribution"
          ),
          
          plotOutput(
            "base_plot",
            height = "300px"
          )
        )
      )
    ),
    
    fluidRow(
      
      column(
        6,
        
        actionButton(
          "back3",
          "← Back",
          class = "back-button"
        )
      ),
      
      column(
        6,
        
        div(
          style = "text-align:right;",
          
          actionButton(
            "next3",
            "Next →",
            class = "next-button"
          )
        )
      )
    )
  ),
  
  
  # ==========================================================
  # PAGE 4 - PATTERN AND CODON ANALYSIS
  # ==========================================================
  
  conditionalPanel(
    
    condition = "output.current_page == 4",
    
    div(
      class = "card",
      
      h2(
        class = "section-title",
        "🔎 Step 3 — Pattern & Codon Analysis"
      ),
      
      h3(
        "🔎 DNA Pattern Explorer"
      ),
      
      p(
        "Search for a nucleotide pattern inside the analyzed DNA sequence."
      ),
      
      fluidRow(
        
        column(
          8,
          
          textInput(
            
            "pattern",
            
            "Enter pattern:",
            
            value = "ATG",
            
            placeholder = "Example: ATG"
          )
        ),
        
        column(
          4,
          
          br(),
          
          actionButton(
            
            "search_pattern",
            
            "🔍 Search Pattern",
            
            class = "search-button"
          )
        )
      ),
      
      h4(
        "Pattern Result"
      ),
      
      textOutput(
        "pattern_count"
      ),
      
      textOutput(
        "pattern_positions"
      ),
      
      DT::DTOutput(
        "pattern_table"
      )
    ),
    
    div(
      class = "card",
      
      h3(
        "🧬 Codon Analyzer"
      ),
      
      p(
        "A codon consists of three consecutive nucleotides."
      ),
      
      fluidRow(
        
        column(
          4,
          
          div(
            class = "stat-box",
            
            div(
              class = "stat-number",
              
              textOutput(
                "codon_count"
              )
            ),
            
            div(
              class = "stat-label",
              "Complete Codons"
            )
          )
        ),
        
        column(
          4,
          
          div(
            class = "stat-box",
            
            div(
              class = "stat-number",
              
              textOutput(
                "start_count"
              )
            ),
            
            div(
              class = "stat-label",
              "Start Codons"
            )
          )
        ),
        
        column(
          4,
          
          div(
            class = "stat-box",
            
            div(
              class = "stat-number",
              
              textOutput(
                "stop_count"
              )
            ),
            
            div(
              class = "stat-label",
              "Stop Codons"
            )
          )
        )
      ),
      
      h4(
        "Codon Table"
      ),
      
      DT::DTOutput(
        "codon_table"
      )
    ),
    
    fluidRow(
      
      column(
        6,
        
        actionButton(
          "back4",
          "← Back",
          class = "back-button"
        )
      ),
      
      column(
        6,
        
        div(
          style = "text-align:right;",
          
          actionButton(
            "next4",
            "Next →",
            class = "next-button"
          )
        )
      )
    )
  ),
  
  
  # ==========================================================
  # PAGE 5 - READING FRAMES
  # ==========================================================
  
  conditionalPanel(
    
    condition = "output.current_page == 5",
    
    div(
      class = "card",
      
      h2(
        class = "section-title",
        "📖 Step 4 — Reading Frame Analyzer"
      ),
      
      p(
        "DNA can be read in different frames. Each reading frame groups nucleotides into different sets of three."
      ),
      
      tabsetPanel(
        
        tabPanel(
          
          "Frame +1",
          
          br(),
          
          h4(
            "Reading Frame +1"
          ),
          
          div(
            class = "sequence-display",
            
            verbatimTextOutput(
              "frame1"
            )
          )
        ),
        
        tabPanel(
          
          "Frame +2",
          
          br(),
          
          h4(
            "Reading Frame +2"
          ),
          
          div(
            class = "sequence-display",
            
            verbatimTextOutput(
              "frame2"
            )
          )
        ),
        
        tabPanel(
          
          "Frame +3",
          
          br(),
          
          h4(
            "Reading Frame +3"
          ),
          
          div(
            class = "sequence-display",
            
            verbatimTextOutput(
              "frame3"
            )
          )
        )
      ),
      
      br(),
      
      div(
        class = "info-box",
        
        strong(
          "What is a reading frame?"
        ),
        
        br(),
        
        "A reading frame determines where the DNA sequence is divided into groups of three nucleotides."
      )
    ),
    
    fluidRow(
      
      column(
        6,
        
        actionButton(
          "back5",
          "← Back",
          class = "back-button"
        )
      ),
      
      column(
        6,
        
        div(
          style = "text-align:right;",
          
          actionButton(
            "next5",
            "Next →",
            class = "next-button"
          )
        )
      )
    )
  ),
  
  
  # ==========================================================
  # PAGE 6 - MUTATION EXPLORER
  # ==========================================================
  
  conditionalPanel(
    
    condition = "output.current_page == 6",
    
    div(
      class = "card",
      
      h2(
        class = "section-title",
        "🧬 Step 5 — Mutation Explorer"
      ),
      
      p(
        "Compare the analyzed DNA sequence with a modified sequence."
      ),
      
      textAreaInput(
        
        "mutated_sequence",
        
        "Enter mutated DNA sequence:",
        
        value = "",
        
        rows = 6,
        
        placeholder = "Example: ATGCGTTCGTTAGCTAGCTAGGCTA"
      ),
      
      actionButton(
        
        "compare_mutation",
        
        "🔬 Compare Sequences",
        
        class = "analyze-button"
      ),
      
      br(),
      br(),
      
      uiOutput(
        "mutation_result"
      )
    ),
    
    fluidRow(
      
      column(
        6,
        
        actionButton(
          "back6",
          "← Back",
          class = "back-button"
        )
      ),
      
      column(
        6,
        
        div(
          style = "text-align:right;",
          
          actionButton(
            "next6",
            "Next →",
            class = "next-button"
          )
        )
      )
    )
  ),
  
  
  # ==========================================================
  # PAGE 7 - FINAL SUMMARY
  # ==========================================================
  
  conditionalPanel(
    
    condition = "output.current_page == 7",
    
    div(
      class = "card",
      
      h2(
        class = "section-title",
        "📈 Step 6 — Final Analysis Summary"
      ),
      
      fluidRow(
        
        column(
          4,
          
          div(
            class = "stat-box",
            
            div(
              class = "stat-number",
              
              textOutput(
                "summary_length"
              )
            ),
            
            div(
              class = "stat-label",
              "Sequence Length"
            )
          )
        ),
        
        column(
          4,
          
          div(
            class = "stat-box",
            
            div(
              class = "stat-number",
              
              textOutput(
                "summary_gc"
              )
            ),
            
            div(
              class = "stat-label",
              "GC Content"
            )
          )
        ),
        
        column(
          4,
          
          div(
            class = "stat-box",
            
            div(
              class = "stat-number",
              
              textOutput(
                "summary_at"
              )
            ),
            
            div(
              class = "stat-label",
              "AT Content"
            )
          )
        )
      ),
      
      hr(),
      
      h4(
        "Analysis Results"
      ),
      
      p(
        strong("Complete Codons: "),
        textOutput(
          "summary_codons",
          inline = TRUE
        )
      ),
      
      p(
        strong("Start Codons: "),
        textOutput(
          "summary_start",
          inline = TRUE
        )
      ),
      
      p(
        strong("Stop Codons: "),
        textOutput(
          "summary_stop",
          inline = TRUE
        )
      ),
      
      p(
        strong("Mutation Result: "),
        textOutput(
          "summary_mutation",
          inline = TRUE
        )
      ),
      
      div(
        class = "info-box",
        
        strong(
          "Analysis Completed"
        ),
        
        br(),
        
        "You have completed the DNA sequence analysis workflow."
      )
    ),
    
    fluidRow(
      
      column(
        4,
        
        actionButton(
          "back7",
          "← Back",
          class = "back-button"
        )
      ),
      
      column(
        4,
        
        div(
          style = "text-align:center;",
          
          actionButton(
            "restart",
            "🔄 Analyze New DNA",
            class = "next-button"
          )
        )
      ),
      
      column(
        4,
        
        br()
      )
    )
  )
  
)


# ============================================================
# SERVER
# ============================================================

server <- function(
    input,
    output,
    session
) {
  
  
  # ==========================================================
  # PAGE NAVIGATION
  # ==========================================================
  
  current_page <- reactiveVal(1)
  
  
  # ---------------- PAGE 1 ----------------
  
  observeEvent(
    input$next1,
    {
      current_page(2)
    }
  )
  
  
  # ---------------- PAGE 2 ----------------
  
  observeEvent(
    input$back2,
    {
      current_page(1)
    }
  )
  
  observeEvent(
    input$next2,
    {
      if (!is.null(dna())) {
        current_page(3)
      } else {
        showNotification(
          "Please analyze and validate your DNA sequence first.",
          type = "error"
        )
      }
    }
  )
  
  
  # ---------------- PAGE 3 ----------------
  
  observeEvent(
    input$back3,
    {
      current_page(2)
    }
  )
  
  observeEvent(
    input$next3,
    {
      if (!is.null(dna())) {
        current_page(4)
      } else {
        showNotification(
          "Please analyze your DNA sequence first.",
          type = "error"
        )
      }
    }
  )
  
  
  # ---------------- PAGE 4 ----------------
  
  observeEvent(
    input$back4,
    {
      current_page(3)
    }
  )
  
  observeEvent(
    input$next4,
    {
      current_page(5)
    }
  )
  
  
  # ---------------- PAGE 5 ----------------
  
  observeEvent(
    input$back5,
    {
      current_page(4)
    }
  )
  
  observeEvent(
    input$next5,
    {
      current_page(6)
    }
  )
  
  
  # ---------------- PAGE 6 ----------------
  
  observeEvent(
    input$back6,
    {
      current_page(5)
    }
  )
  
  observeEvent(
    input$next6,
    {
      current_page(7)
    }
  )
  
  
  # ---------------- PAGE 7 ----------------
  
  observeEvent(
    input$back7,
    {
      current_page(6)
    }
  )
  
  observeEvent(
    input$restart,
    {
      current_page(2)
    }
  )
  
  
  # ==========================================================
  # SEND CURRENT PAGE TO UI
  # ==========================================================
  
  output$current_page <- reactive({
    current_page()
  })
  
  outputOptions(
    output,
    "current_page",
    suspendWhenHidden = FALSE
  )
  
  
  # ==========================================================
  # STEP INDICATOR
  # ==========================================================
  
  output$step_indicator <- renderUI({
    
    page <- current_page()
    
    if (page == 1) {
      
      HTML(
        '<div class="step-indicator">
           <span class="step-text">Welcome</span>
         </div>'
      )
      
    } else {
      
      HTML(
        paste0(
          '<div class="step-indicator">
             <span class="step-text">
             Step ',
          page - 1,
          ' of 6
             </span>
           </div>'
        )
      )
    }
  })
  
  
  # ==========================================================
  # DNA VALIDATION
  # ==========================================================
  
  dna <- eventReactive(
    
    input$analyze,
    
    {
      
      sequence <- input$dna_sequence
      
      
      # ------------------------------------------------------
      # FASTA FILE
      # ------------------------------------------------------
      
      if (
        !is.null(input$fasta_file)
      ) {
        
        fasta_data <- readLines(
          input$fasta_file$datapath
        )
        
        fasta_data <- fasta_data[
          !grepl(
            "^>",
            fasta_data
          )
        ]
        
        sequence <- paste(
          fasta_data,
          collapse = ""
        )
      }
      
      
      # ------------------------------------------------------
      # CLEAN SEQUENCE
      # ------------------------------------------------------
      
      sequence <- toupper(
        sequence
      )
      
      sequence <- gsub(
        "\\s+",
        "",
        sequence
      )
      
      
      # ------------------------------------------------------
      # EMPTY CHECK
      # ------------------------------------------------------
      
      if (
        sequence == ""
      ) {
        
        showNotification(
          "Please enter a DNA sequence.",
          type = "error"
        )
        
        return(NULL)
      }
      
      
      # ------------------------------------------------------
      # VALID DNA CHECK
      # ------------------------------------------------------
      
      if (
        !grepl(
          "^[ATGC]+$",
          sequence
        )
      ) {
        
        showNotification(
          "Invalid DNA sequence. Only A, T, G and C are allowed.",
          type = "error"
        )
        
        return(NULL)
      }
      
      
      sequence
    }
  )
  
  
  # ==========================================================
  # VALIDATION MESSAGE
  # ==========================================================
  
  output$validation_message <- renderUI({
    
    req(
      dna()
    )
    
    div(
      class = "info-box",
      
      strong(
        "✓ DNA sequence validated successfully."
      ),
      
      br(),
      
      paste(
        "Length:",
        nchar(dna()),
        "nucleotides"
      )
    )
  })
  
  
  # ==========================================================
  # SEQUENCE LENGTH
  # ==========================================================
  
  output$sequence_length <- renderText({
    
    req(
      dna()
    )
    
    nchar(
      dna()
    )
  })
  
  
  # ==========================================================
  # GC CONTENT
  # ==========================================================
  
  output$gc_content <- renderText({
    
    req(
      dna()
    )
    
    sequence <- dna()
    
    bases <- strsplit(
      sequence,
      ""
    )[[1]]
    
    gc_count <- sum(
      bases %in% c(
        "G",
        "C"
      )
    )
    
    gc_percentage <- (
      gc_count /
        length(bases)
    ) * 100
    
    paste0(
      round(
        gc_percentage,
        2
      ),
      "%"
    )
  })
  
  
  # ==========================================================
  # AT CONTENT
  # ==========================================================
  
  output$at_content <- renderText({
    
    req(
      dna()
    )
    
    sequence <- dna()
    
    bases <- strsplit(
      sequence,
      ""
    )[[1]]
    
    at_count <- sum(
      bases %in% c(
        "A",
        "T"
      )
    )
    
    at_percentage <- (
      at_count /
        length(bases)
    ) * 100
    
    paste0(
      round(
        at_percentage,
        2
      ),
      "%"
    )
  })
  
  
  # ==========================================================
  # VALIDATED SEQUENCE
  # ==========================================================
  
  output$validated_sequence <- renderText({
    
    req(
      dna()
    )
    
    dna()
  })
  
  
  # ==========================================================
  # NUCLEOTIDE TABLE
  # ==========================================================
  
  output$base_table <- DT::renderDT({
    
    req(
      dna()
    )
    
    sequence <- dna()
    
    bases <- c(
      "A",
      "T",
      "G",
      "C"
    )
    
    counts <- sapply(
      
      bases,
      
      function(base) {
        
        sum(
          strsplit(
            sequence,
            ""
          )[[1]] == base
        )
      }
    )
    
    percentages <- (
      counts /
        nchar(sequence)
    ) * 100
    
    data.frame(
      
      Nucleotide = bases,
      
      Count = counts,
      
      Percentage =
        round(
          percentages,
          2
        )
    )
    
  },
  
  options = list(
    
    paging = FALSE,
    
    searching = FALSE,
    
    info = FALSE
    
  ))
  
  
  # ==========================================================
  # NUCLEOTIDE GRAPH
  # ==========================================================
  
  output$base_plot <- renderPlot({
    
    req(
      dna()
    )
    
    sequence <- dna()
    
    bases <- c(
      "A",
      "T",
      "G",
      "C"
    )
    
    counts <- sapply(
      
      bases,
      
      function(base) {
        
        sum(
          strsplit(
            sequence,
            ""
          )[[1]] == base
        )
      }
    )
    
    plot_data <- data.frame(
      
      Nucleotide = bases,
      
      Count = counts
    )
    
    ggplot(
      
      plot_data,
      
      aes(
        x = Nucleotide,
        y = Count
      )
      
    ) +
      
      geom_col() +
      
      labs(
        
        title =
          "DNA Nucleotide Distribution",
        
        x =
          "Nucleotide",
        
        y =
          "Count"
        
      ) +
      
      theme_minimal()
    
  })
  
  
  # ==========================================================
  # PATTERN SEARCH
  # ==========================================================
  
  pattern_result <- eventReactive(
    
    input$search_pattern,
    
    {
      
      req(
        dna()
      )
      
      sequence <- dna()
      
      pattern <- toupper(
        input$pattern
      )
      
      pattern <- gsub(
        "\\s+",
        "",
        pattern
      )
      
      
      # ------------------------------------------------------
      # Validate pattern
      # ------------------------------------------------------
      
      if (
        pattern == ""
      ) {
        
        showNotification(
          "Please enter a pattern.",
          type = "error"
        )
        
        return(NULL)
      }
      
      
      if (
        !grepl(
          "^[ATGC]+$",
          pattern
        )
      ) {
        
        showNotification(
          "Pattern must contain only A, T, G and C.",
          type = "error"
        )
        
        return(NULL)
      }
      
      
      # ------------------------------------------------------
      # Find positions
      # ------------------------------------------------------
      
      pattern_length <- nchar(
        pattern
      )
      
      sequence_length <- nchar(
        sequence
      )
      
      positions <- c()
      
      
      if (
        pattern_length <=
        sequence_length
      ) {
        
        for (
          i in 1:
          (
            sequence_length -
            pattern_length +
            1
          )
        ) {
          
          current_part <- substr(
            
            sequence,
            
            i,
            
            i +
              pattern_length -
              1
          )
          
          if (
            current_part ==
            pattern
          ) {
            
            positions <- c(
              positions,
              i
            )
          }
        }
      }
      
      
      list(
        
        pattern = pattern,
        
        positions = positions
      )
    }
  )
  
  
  # ==========================================================
  # PATTERN COUNT
  # ==========================================================
  
  output$pattern_count <- renderText({
    
    result <- pattern_result()
    
    req(
      result
    )
    
    count <- length(
      result$positions
    )
    
    paste(
      "Pattern:",
      result$pattern,
      "| Occurrences:",
      count
    )
  })
  
  
  # ==========================================================
  # PATTERN POSITIONS
  # ==========================================================
  
  output$pattern_positions <- renderText({
    
    result <- pattern_result()
    
    req(
      result
    )
    
    if (
      length(
        result$positions
      ) == 0
    ) {
      
      return(
        "No positions found."
      )
    }
    
    paste(
      
      "Positions:",
      
      paste(
        result$positions,
        collapse = ", "
      )
    )
  })
  
  
  # ==========================================================
  # PATTERN TABLE
  # ==========================================================
  
  output$pattern_table <- DT::renderDT({
    
    result <- pattern_result()
    
    req(
      result
    )
    
    if (
      length(
        result$positions
      ) == 0
    ) {
      
      return(
        data.frame(
          Message = "Pattern not found"
        )
      )
    }
    
    positions <- result$positions
    
    data.frame(
      
      Occurrence =
        seq_along(
          positions
        ),
      
      Pattern =
        result$pattern,
      
      Start_Position =
        positions,
      
      End_Position =
        positions +
        nchar(
          result$pattern
        ) -
        1
    )
    
  })
  
  
  # ==========================================================
  # CODON GENERATOR
  # ==========================================================
  
  codons <- reactive({
    
    req(
      dna()
    )
    
    sequence <- dna()
    
    sequence_length <- nchar(
      sequence
    )
    
    number_of_codons <-
      floor(
        sequence_length /
          3
      )
    
    if (
      number_of_codons == 0
    ) {
      
      return(
        character(0)
      )
    }
    
    codon_vector <- character(
      number_of_codons
    )
    
    for (
      i in seq_len(
        number_of_codons
      )
    ) {
      
      start_position <-
        (
          (i - 1) * 3
        ) + 1
      
      end_position <-
        start_position + 2
      
      codon_vector[i] <-
        substr(
          
          sequence,
          
          start_position,
          
          end_position
        )
    }
    
    codon_vector
  })
  
  
  # ==========================================================
  # CODON COUNT
  # ==========================================================
  
  output$codon_count <- renderText({
    
    req(
      dna()
    )
    
    length(
      codons()
    )
  })
  
  
  # ==========================================================
  # START CODON COUNT
  # ==========================================================
  
  output$start_count <- renderText({
    
    codon_vector <- codons()
    
    sum(
      codon_vector == "ATG"
    )
  })
  
  
  # ==========================================================
  # STOP CODON COUNT
  # ==========================================================
  
  output$stop_count <- renderText({
    
    codon_vector <- codons()
    
    sum(
      codon_vector %in%
        c(
          "TAA",
          "TAG",
          "TGA"
        )
    )
  })
  
  
  # ==========================================================
  # CODON TABLE
  # ==========================================================
  
  output$codon_table <- DT::renderDT({
    
    codon_vector <- codons()
    
    req(
      length(codon_vector) > 0
    )
    
    codon_type <- ifelse(
      
      codon_vector ==
        "ATG",
      
      "Start Codon",
      
      ifelse(
        
        codon_vector %in%
          c(
            "TAA",
            "TAG",
            "TGA"
          ),
        
        "Stop Codon",
        
        "Regular Codon"
      )
    )
    
    data.frame(
      
      Position =
        seq_along(
          codon_vector
        ),
      
      Codon =
        codon_vector,
      
      Type =
        codon_type
    )
    
  })
  
  
  # ==========================================================
  # READING FRAME FUNCTION
  # ==========================================================
  
  create_frame <- function(
    sequence,
    frame
  ) {
    
    sequence_length <- nchar(
      sequence
    )
    
    if (
      frame == 1
    ) {
      
      start <- 1
      
    } else if (
      frame == 2
    ) {
      
      start <- 2
      
    } else {
      
      start <- 3
    }
    
    if (
      start > sequence_length
    ) {
      
      return(
        "Sequence is too short for this reading frame."
      )
    }
    
    remaining <- substr(
      
      sequence,
      
      start,
      
      sequence_length
    )
    
    remaining_length <- nchar(
      remaining
    )
    
    number_of_codons <-
      floor(
        remaining_length /
          3
      )
    
    if (
      number_of_codons == 0
    ) {
      
      return(
        remaining
      )
    }
    
    frame_codons <- character(
      number_of_codons
    )
    
    for (
      i in seq_len(
        number_of_codons
      )
    ) {
      
      codon_start <-
        (
          (i - 1) * 3
        ) + 1
      
      codon_end <-
        codon_start + 2
      
      frame_codons[i] <-
        substr(
          
          remaining,
          
          codon_start,
          
          codon_end
        )
    }
    
    paste(
      
      frame_codons,
      
      collapse = " - "
    )
  }
  
  
  # ==========================================================
  # FRAME +1
  # ==========================================================
  
  output$frame1 <- renderText({
    
    req(
      dna()
    )
    
    create_frame(
      dna(),
      1
    )
  })
  
  
  # ==========================================================
  # FRAME +2
  # ==========================================================
  
  output$frame2 <- renderText({
    
    req(
      dna()
    )
    
    create_frame(
      dna(),
      2
    )
  })
  
  
  # ==========================================================
  # FRAME +3
  # ==========================================================
  
  output$frame3 <- renderText({
    
    req(
      dna()
    )
    
    create_frame(
      dna(),
      3
    )
  })
  
  
  # ==========================================================
  # MUTATION COMPARISON
  # ==========================================================
  
  mutation_result <- eventReactive(
    
    input$compare_mutation,
    
    {
      
      req(
        dna()
      )
      
      original <- dna()
      
      mutated <- toupper(
        input$mutated_sequence
      )
      
      mutated <- gsub(
        "\\s+",
        "",
        mutated
      )
      
      
      # ------------------------------------------------------
      # Empty check
      # ------------------------------------------------------
      
      if (
        mutated == ""
      ) {
        
        return(
          list(
            valid = FALSE,
            message = "Please enter a mutated DNA sequence."
          )
        )
      }
      
      
      # ------------------------------------------------------
      # Validate mutated sequence
      # ------------------------------------------------------
      
      if (
        !grepl(
          "^[ATGC]+$",
          mutated
        )
      ) {
        
        return(
          list(
            valid = FALSE,
            message = "Mutated sequence must contain only A, T, G and C."
          )
        )
      }
      
      
      # ------------------------------------------------------
      # Compare lengths
      # ------------------------------------------------------
      
      original_length <- nchar(
        original
      )
      
      mutated_length <- nchar(
        mutated
      )
      
      
      # ------------------------------------------------------
      # Same length = substitution comparison
      # ------------------------------------------------------
      
      if (
        original_length ==
        mutated_length
      ) {
        
        original_bases <- strsplit(
          original,
          ""
        )[[1]]
        
        mutated_bases <- strsplit(
          mutated,
          ""
        )[[1]]
        
        differences <- which(
          original_bases != mutated_bases
        )
        
        
        if (
          length(differences) == 0
        ) {
          
          return(
            list(
              valid = TRUE,
              type = "No Mutation",
              position = NA,
              original_base = "-",
              mutated_base = "-",
              original = original,
              mutated = mutated
            )
          )
        }
        
        
        return(
          list(
            valid = TRUE,
            type = "Substitution",
            position = differences,
            original_base = original_bases[differences],
            mutated_base = mutated_bases[differences],
            original = original,
            mutated = mutated
          )
        )
      }
      
      
      # ------------------------------------------------------
      # Longer mutated sequence = insertion
      # ------------------------------------------------------
      
      if (
        mutated_length >
        original_length
      ) {
        
        difference <- mutated_length -
          original_length
        
        return(
          list(
            valid = TRUE,
            type = "Insertion",
            position = NA,
            original_base = "-",
            mutated_base = paste0(
              "+",
              difference,
              " nucleotide(s)"
            ),
            original = original,
            mutated = mutated
          )
        )
      }
      
      
      # ------------------------------------------------------
      # Shorter mutated sequence = deletion
      # ------------------------------------------------------
      
      if (
        mutated_length <
        original_length
      ) {
        
        difference <- original_length -
          mutated_length
        
        return(
          list(
            valid = TRUE,
            type = "Deletion",
            position = NA,
            original_base = paste0(
              "-",
              difference,
              " nucleotide(s)"
            ),
            mutated_base = "-",
            original = original,
            mutated = mutated
          )
        )
      }
    }
  )
  
  
  # ==========================================================
  # MUTATION RESULT DISPLAY
  # ==========================================================
  
  output$mutation_result <- renderUI({
    
    result <- mutation_result()
    
    req(
      result
    )
    
    
    if (
      !result$valid
    ) {
      
      return(
        div(
          class = "notice",
          
          strong(
            "⚠️ "
          ),
          
          result$message
        )
      )
    }
    
    
    if (
      result$type ==
      "No Mutation"
    ) {
      
      return(
        div(
          class = "info-box",
          
          strong(
            "✓ No mutation detected"
          ),
          
          br(),
          
          "The original and mutated sequences are identical."
        )
      )
    }
    
    
    position_text <- ""
    
    if (
      !all(
        is.na(
          result$position
        )
      )
    ) {
      
      position_text <- paste(
        result$position,
        collapse = ", "
      )
    }
    
    
    div(
      
      class = "mutation-box",
      
      h4(
        "🔬 Mutation Detected"
      ),
      
      p(
        strong("Mutation Type: "),
        result$type
      ),
      
      if (
        position_text != ""
      ) {
        p(
          strong("Position(s): "),
          position_text
        )
      },
      
      p(
        strong("Original: "),
        result$original
      ),
      
      p(
        strong("Mutated: "),
        result$mutated
      ),
      
      p(
        strong("Original Base: "),
        paste(
          result$original_base,
          collapse = ", "
        )
      ),
      
      p(
        strong("Mutated Base: "),
        paste(
          result$mutated_base,
          collapse = ", "
        )
      )
    )
  })
  
  
  # ==========================================================
  # FINAL SUMMARY
  # ==========================================================
  
  output$summary_length <- renderText({
    
    req(
      dna()
    )
    
    nchar(
      dna()
    )
  })
  
  
  output$summary_gc <- renderText({
    
    req(
      dna()
    )
    
    sequence <- dna()
    
    bases <- strsplit(
      sequence,
      ""
    )[[1]]
    
    gc_count <- sum(
      bases %in%
        c(
          "G",
          "C"
        )
    )
    
    paste0(
      round(
        gc_count /
          length(bases) *
          100,
        2
      ),
      "%"
    )
  })
  
  
  output$summary_at <- renderText({
    
    req(
      dna()
    )
    
    sequence <- dna()
    
    bases <- strsplit(
      sequence,
      ""
    )[[1]]
    
    at_count <- sum(
      bases %in%
        c(
          "A",
          "T"
        )
    )
    
    paste0(
      round(
        at_count /
          length(bases) *
          100,
        2
      ),
      "%"
    )
  })
  
  
  output$summary_codons <- renderText({
    
    req(
      dna()
    )
    
    length(
      codons()
    )
  })
  
  
  output$summary_start <- renderText({
    
    req(
      dna()
    )
    
    codon_vector <- codons()
    
    sum(
      codon_vector ==
        "ATG"
    )
  })
  
  
  output$summary_stop <- renderText({
    
    req(
      dna()
    )
    
    codon_vector <- codons()
    
    sum(
      codon_vector %in%
        c(
          "TAA",
          "TAG",
          "TGA"
        )
    )
  })
  
  
  output$summary_mutation <- renderText({
    
    result <- mutation_result()
    
    if (
      is.null(result)
    ) {
      
      return(
        "Not analyzed"
      )
    }
    
    if (
      !result$valid
    ) {
      
      return(
        "Invalid mutation sequence"
      )
    }
    
    result$type
  })
  
}


# ============================================================
# RUN APPLICATION
# ============================================================

shinyApp(
  
  ui = ui,
  
  server = server
  
)
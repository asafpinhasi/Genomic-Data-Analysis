setwd("C:/Users/KerenYlab/Asaf/R Genomic Data Analysis/Final_project")
seurat <- readRDS("seurat.filtered")
cell_type_markers <- readRDS("cell_type_markers")
plots <- readRDS("enrichment_plots_by_cluster")
confusion_matrix <- readRDS("confusion_matrix")
auc_curve <- readRDS("ROC_curves")
feature_importace_matrix <- readRDS("feature_importance_matrix")
library(xgboost)
library(ggpubr)


library(ggplot2)
no_plot <- ggplot() +                      # Draw ggplot2 plot with text only
  annotate("text",
           x = 1,
           y = 1,
           size = 8,
           label = "No differentially expressed genes from this category") + 
  theme_void()



ui <- fluidPage(
  
  # App title
  titlePanel("My Shiny App"),
  
  # Tabs
  tabsetPanel(
    
    # Tab 1 - Feature plot
    tabPanel(
      "Feature plot",
      sidebarLayout(
        sidebarPanel(
          selectInput("cluster", "Select a cluster", choices = names(cell_type_markers))
        ),
        mainPanel(
          plotOutput("feature_plot")
        )
      )
    ),
    
    # Tab 2 - Enrichment analysis
    tabPanel(
      "Enrichment analysis",
      sidebarLayout(
        sidebarPanel(
          selectInput("cluster2", "Choose a cluster:", choices = names(plots), selected = 1),
          selectInput("ontology", "Choose an ontology group:", choices = names(plots[[1]]), selected = plots[[1]][[1]]),
          selectInput("plot_type", "Choose a plot type:", choices = c("dotplot", "emapplot"), selected = "dotplot")
        ),
        mainPanel(
          plotOutput("enrichment_plot")
        )
      )
    ),
    
    # Tab 3 displays model evaluation
    tabPanel("Supervised ML",
       sidebarLayout(
         sidebarPanel(
           radioButtons("supervised_ML", "Select plot type:",
                        c("Confusion Matrix and AUC Curve", "Feature Importance Graph")
           )
         ),
         mainPanel(
           plotOutput("supervised_plot")
         )
       )
    )
  )
)




# Define server
server <- function(input, output) {
  
  # Feature plot function
  output$feature_plot <- renderPlot({
    FeaturePlot(seurat, reduction = "tsne", features = cell_type_markers[[input$cluster]], pt.size = 0.6, min.cutoff = 'q10')+
      DimPlot(seurat, reduction = "tsne", group.by = "anno",pt.size = 0.6, label = T)
  })
  
  output$enrichment_plot <- renderPlot({
    cluster <- input$cluster2
    ont <- input$ontology
    plot_type <- input$plot_type
    # Render plot
    if (length(plots[[cluster]][[ont]]) == 2) {
      plots[[cluster]][[ont]][[plot_type]]
    }
    else{
      no_plot
    }
  })

  output$supervised_plot <- renderPlot({
    print("hello")
    print(input$supervised_ML)
    if (input$supervised_ML == "Confusion Matrix and AUC Curve") {
      cmplot <- confusion_matrix
      acplot <- auc_curve
      ggarrange(acplot, cmplot, 
                labels = c("A", "B"),
                ncol = 1, nrow = 2)
    } else if (input$supervised_ML == "Feature Importance Graph") {
      xgb.plot.importance(feature_importace_matrix)
    }
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)


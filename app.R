library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinyjs)
library(bslib)
library(bsicons)
library(htmltools)
library(shinycssloaders)
library(metathis)

library(dplyr)
library(DT)
library(fst)
library(rlang)

library(Rtsne)
library(dbscan)
library(cluster)
library(keras)  # For Autoencoder
library(uwot)   # For UMAP
library(factoextra)
library(fpc)
library(forcats)

library(kableExtra)

library(plotly)
library(ggplot2)
library(ggalt)
library(viridis)
library(RColorBrewer)


source("www/scripts/theme_Publication.R")
source("www/scripts/run_models.R")


anim_width <- function(x, width1, width2) {
  x |> tagAppendAttributes(
    class = "animate-width",
    style = css(
      `--width1` = validateCssUnit(width1),
      `--width2` = validateCssUnit(width2),
    ),
  )
}

# Sample structure of results_df2 for context
# load("data/results_df2.Rda")
performance <- fst::read_fst("data/performance.fst")
load("data/features.RData")

best_params <- performance %>%
  group_by(dim_red_method, cluster_method) %>%
  filter(!is.na(silhouette_score)) %>%
  slice_max(order_by = silhouette_score, n = 1, with_ties = FALSE) %>%
  ungroup()

# Method-parameter mapping
dim_red_method_parameters <- list(
  None = character(0),
  PCA = c("n_components"),
  tSNE = c("perplexity"),
  Autoencoder = c("encoding_dim", "epochs"),
  UMAP = c("n_components", "n_neighbors", "min_dist")
)

dim_red_method_labels <- list(
  n_components = tags$span(
    "Number of Components: ",
    popover(
      bsicons::bs_icon("info-circle"),
      title = "Number of Components",
      "Determines the number of components to retain in the analysis. A higher number of components captures more variance but may include noise, whereas fewer components simplify the model but might miss important variance."
    )
  ),
  perplexity = tags$span(
    "Perplexity (tSNE):",
    popover(
      bsicons::bs_icon("info-circle"),
      title = "Perplexity (tSNE)",
      "Controls the balance between local and global aspects of your data in the tSNE model. Higher values consider a broader context, while lower values focus on local neighbors."
    )
  ),
  encoding_dim = tags$span(
    "Encoding Dimensions (Autoencoder):",
    popover(
      bsicons::bs_icon("info-circle"),
      title = "Encoding Dimensions (Autoencoder)",
      "Specifies the size of the encoding layer, which represents the compressed knowledge of the input data. Fewer dimensions force the autoencoder to learn a more compact representation, while more dimensions allow for a more detailed encoding."
    )
  ),
  epochs = tags$span(
    "Epochs (Autoencoder):",
    popover(
      bsicons::bs_icon("info-circle"),
      title = "Epochs (Autoencoder)",
      "Defines the number of times the learning algorithm will work through the entire training dataset. More epochs can lead to a better model but increase the risk of overfitting."
    )
  ),
  n_neighbors = tags$span(
    "N Neighbors (UMAP):",
    popover(
      bsicons::bs_icon("info-circle"),
      title = "Number of Neighbors (UMAP)",
      "Determines the number of neighboring points used in the UMAP's local manifold approximations. A higher value captures more of the global structure, while a lower value focuses on the local neighborhood."
    )
  ),
  min_dist = tags$span(
    "min dist (UMAP):",
    popover(
      bsicons::bs_icon("info-circle"),
      title = "Minimum Distance (UMAP)",
      "Controls the minimum distance apart that points are allowed to be in the low-dimensional representation. Smaller values allow points to cluster more tightly in low-dimensional space."
    )
  )
)

cluster_method_parameters <- list(
  kmeans = c("k"),
  DBSCAN = c("eps", "minPts"),
  hclust = c("k", "linkage")
)

cluster_method_labels <- list(
  k = tags$span(
    "Number of Clusters: ", 
    popover(
      bsicons::bs_icon("info-circle"),
      title = "Number of Clusters",
      "Specifies the number of clusters to form. The number of clusters heavily influences the results."
    )
  ),
  eps = tags$span(
    "Epsilon (DBSCAN):", 
    popover(
      bsicons::bs_icon("info-circle"),
      title = "Epsilon (DBSCAN)",
      "Defines the maximum distance between two samples for them to be considered as in the same neighborhood. A smaller value results in a higher density necessary to form a cluster."
    )
  ),
  minPts = tags$span(
    "Minimum Points (DBSCAN)", 
    popover(
      bsicons::bs_icon("info-circle"),
      title = "Minimum Points (DBSCAN)",
      "The minimum number of points required to form a dense region. Increasing this value demands more points to form a cluster, potentially resulting in more noise points."
    )
  ),
  linkage = tags$span(
    "Linkage Criteria (Hierarchical)", 
    popover(
      bsicons::bs_icon("info-circle"),
      title = "Linkage Criteria (Hierarchical)",
      "Defines the metric used to measure the distance between sets of observations in hierarchical clustering. Common methods include 'single', 'complete', 'average', and 'Ward's method', each affecting the cluster formation differently."
    )
  )
)


method_parameter_map <- list(
  None = character(0),
  PCA = c("n_components"),
  tSNE = c("perplexity"),
  Autoencoder = c("encoding_dim", "epochs"),
  UMAP = c("n_components_umap", "n_neighbors", "min_dist"),
  kmeans = c("k"),
  DBSCAN = c("eps", "minPts"),
  hclust = c("k_hclust", "linkage")
)

# definition of parameter values
params_values <- list(
  n_components = c(2, 3, 5, 10, 15, 20, 25, 27, 30, 35),  # For PCA and UMAP
  perplexity = c(5, 10, 20, 30, 40, 50),  # For t-SNE
  encoding_dim = c(8, 16, 24, 32, 48, 64),  # For Autoencoder
  epochs = c(50, 100, 150, 200),  # For Autoencoder, training epochs
  n_neighbors = c(5, 10, 15, 20, 30),  # For UMAP
  min_dist = c(0.1, 0.25, 0.5, 0.75),  # For UMAP
  k = c(2, 3, 4, 5, 6, 7, 8, 9, 10),  # For k-means and hclust
  eps = c(0.1, 0.2, 0.3, 0.5, 0.7, 1.0, 1.5, 2),  # For DBSCAN
  minPts = c(2, 3, 4, 5),  # For DBSCAN
  linkage = c("ward.D2", "average", "single", "complete")  # For hclust
)

# Define UI
ui <- page_navbar(
  title = "tsFeatureClustR", 
  id = "main",
  fillable = "Model Explorer",
  
  theme = bs_theme(bootswatch = "litera"), # bootswatch = "litera", lux, lumen
  useShinyjs(),
  
  tags$head(
    tags$link(rel = "shortcut icon", href = "favicon.ico"),
    tags$style(HTML("
      p {
        font-family: 'Lucida Grande', serif; /* Use Lucida Grande, falling back to any serif font */
        font-size: 16px; /* Match the font size */
        color: text-body; /* Adjust as needed */
        user-select: none; /* Apply the user-select rule for consistency */
        -webkit-user-select: none; /* For Safari */
        -moz-user-select: none; /* For Firefox */
        -ms-user-select: none; /* For IE/Edge */
      }
      a {
        font-family: 'Lucida Grande', serif; /* Ensure links match the text */
        font-size: 16px;
      }
      .center-message {
        display: flex;
        justify-content: center;
        align-items: center;
        height: 100%;
      }
      .card-img-container img {
        display: block;
        margin-left: auto;
        margin-right: auto;
        width: 250px; /* Adjust based on your image size */
      }
    ")),
    tags$script(HTML('
        var fakeClick = function(tabName) {
          var dropdownList = document.getElementsByTagName("a");
          for (var i = 0; i < dropdownList.length; i++) {
            var link = dropdownList[i];
            if(link.getAttribute("data-value") == tabName) {
              link.click();
            };
          }
        };
      ')),
    tags$meta(name="twitter:title", content="TsFeatureClustR WebApp"),
    tags$meta(name="twitter:description", content="An interactive webapp to explore and understand feature-based time-series clustering."),
    tags$meta(name="twitter:url", content="TsFeatureClustR WebApp"),
    tags$meta(name="twitter:image", content="media.png"),
    tags$meta(name="twitter:image:alt", content="TsFeatureClustR WebApp"),
    tags$meta(name="twitter:card", content="summary"),
    tags$meta(name="twitter:creator", content="@JannisWrites"),
    tags$meta(name="twitter:site", content="@JannisWrites")
  ),
  
  nav_panel("Welcome",
            fluidRow(
              column(width = 8,
                     card_body(
                       padding = 50,
                       # class = "p-3",
                       tags$h3("Welcome to tsFeatureClustR webapp!"),
                       "Welcome to tsFeatureClustR, your interactive dashboard for exploring and understanding time series feature clustering. This web application is designed to facilitate the exploration of different clustering algorithms and dimensionality reduction techniques on a predefined dataset of time series features. Whether you are a researcher, student, or data enthusiast, tsFeatureClustR offers a hands-on experience that enhances your understanding of time series data clustering and its underlying processes.",
                       tags$h4("How to Get Started:"),
                       tags$ul(
                         tags$li(tags$strong(a("Model Performance:", onclick = "fakeClick('Model Performance')", href="#")), " This panel is your gateway to evaluate the performance of various precalculated models. By selecting different algorithms for dimensionality reduction and clustering, you can visualize the model performances through a performance plot. This feature allows for a comparative insight into how different parameter settings affect the silhouette score, guiding you to optimal clustering strategies without the need for manual computation."),
                         tags$li(tags$strong(a("Model Explorer:", onclick = "fakeClick('Model Explorer')", href="#")), " For those who wish to delve deeper and customize their analysis, the Model Explorer panel provides the tools to do so. Here, you can select from a range of dimensionality reduction and clustering algorithms, adjust key parameters, and run the model on the dataset in real-time. This panel encourages experimentation and offers a hands-on approach to data analysis, enabling you to discover the most effective strategies for your specific needs.")
                       ),
                       tags$h4("User Interface Highlights:"),
                       tags$ul(
                         tags$li("Control Panels: Customize your analysis by selecting different algorithms and their parameters. The UI is designed to be intuitive, allowing easy navigation between different options and configurations."),
                         tags$li("Interactive Results Display: Visualize the results through dynamic plots and tables that update based on your selections and analyses. This immediate feedback loop enhances understanding and aids in the identification of the most effective clustering strategies."),
                         tags$li("Performance Insights: Leverage precalculated model performances to quickly gauge the effectiveness of various algorithm-parameter combinations. This feature saves time and computational resources, providing a solid starting point for further exploration.")
                       ),
                       tags$h4("Getting the Most Out of tsFeatureClustR:"),
                       tags$ul(
                         tags$li("Experiment with Different Algorithms: Don't hesitate to try out various combinations of dimensionality reduction and clustering algorithms. Each has its strengths and can reveal different aspects of the data."),
                         tags$li("Adjust Parameters: Fine-tune the parameters for each algorithm to see how they influence the model's performance. This exploration can lead to deeper insights into the dataset and the behavior of the algorithms."),
                         tags$li("Consult the Performance Plot: Use the precalculated performance plot in the Model Performance panel to guide your explorations in the Model Explorer panel. This strategy can help you identify promising algorithm-parameter combinations more quickly.")
                       ),
                       "Dive in, experiment, and explore the time series feature clustering process with the tsFeatureClustR webapp."
                     )
              ),
              column(width = 4,
                     card(
                       fill = FALSE,
                       div(class = "card-img-container",
                         card_image(
                           width = 250,
                           file = "www/images/logo.png",
                           href = "https://www.tsfeatureclustr.com/"
                         )
                       ),
                       card_body(
                         fill = FALSE,
                         card_title("Part of the TsFeatureClustR Illustration"),
                         tags$p("This interactive web application accompanies the ", 
                                tags$a(href = "https://www.tsfeatureclustr.com/", target = "_blank", "TsFeatureClustR illustration"),
                                " and relies on the data that has been prepared as part of this illustration. All data is publicly available as part of the academic journal publication, for which we developed this illustration."
                         )
                         # HTML("This interactive web application accompanies the <a href='https://www.tsfeatureclustr.com/' target='_blank'>TsFeatureClustR illustration</a> and relies on the data that has been prepared as part of this illustration. All data is publicly available as part of the academic journal publication, for which we developed this illustration.")
                       )
                     ),
                     value_box(
                       title = "Model Combinations",
                       value = length(dim_red_method_parameters) * length(cluster_method_parameters),
                       showcase = bs_icon("diagram-3"),
                       # showcase_layout = "top right",
                       theme = "primary",
                       "Try out diverse models"
                     ),
                     value_box(
                       title = "Interactive Parameters",
                       value = length(unlist(method_parameter_map))+2,
                       showcase = bs_icon("sliders"),
                       # showcase_layout = "top right",
                       theme = "primary",
                       "Turn nobs to explore"
                     ),
                     value_box(
                       title = "Precalculated Model",
                       value = format(nrow(performance), big.mark=","),
                       showcase = bs_icon("speedometer"),
                       theme = "primary",
                       "Instantly compare the performance of common models"
                     )
              )
              )
  ),
  nav_panel("Model Performance",
            tabName = "performance",
           conditionalPanel(
             condition = 'input.dimRedMethod === null || input.dimRedMethod.length === 0 || input.clusterMethod === null || input.clusterMethod.length === 0',
             div(class = "center-message", h4("Please make a selection to see the performance plot."), style = "height: 600px;")
           ),
           
           # Performance plot, shown only when selections have been made
           conditionalPanel(
             condition = 'input.dimRedMethod !== null && input.dimRedMethod.length > 0 && input.clusterMethod !== null && input.clusterMethod.length > 0',
             card(
               card_header("Performance Plot"),
               height = 700,
               full_screen = TRUE,
               card_body(
                 tags$div(
                   "To assess the performance of the clustering algorithms with the parameter ranges you have selected, we display the ",
                   tags$i("Average Silhouette Width"),
                   popover(
                     bsicons::bs_icon("info-circle"),
                     title = "Average Silhouette Width",
                     "This metric provides a measure of how close each point in one cluster is to points in the neighboring clusters. Higher silhouette width indicates better separation and cohesion."
                   ),
                   " to compare the model performances. The best performing model per algorithm combination is marked with a green border (maximum value). The worst performing model is marked with a light blue border (minimum value)."
                 ),
                plotOutput("heatmapPlot") %>% withSpinner(color="#0d6efd")
               )
             )
           ),
           
           card(
             fill = TRUE,
             layout_column_wrap(
               fill = TRUE,
               fillable = TRUE,
               width = 1/2,
               # class = "p-3",
               card_body(
                 fillable = TRUE,
                 min_height = 350,
                 pickerInput(
                   "dimRedMethod",
                   "Dimension Reduction Method",
                   choices = names(dim_red_method_parameters),
                   width = "95%",
                   multiple = TRUE,
                   options = pickerOptions(
                     actionsBox = TRUE, 
                     size = 10,
                     selectedTextFormat = "count > 3"
                   )
                 ),
                 uiOutput("dimRedParams", width = "auto")
               ),
               card_body(
                 fillable = TRUE,
                 min_height = 350,
                 pickerInput(
                   "clusterMethod",
                   "Clustering Method",
                   choices = names(cluster_method_parameters),
                   width = "95%",
                   multiple = TRUE,
                   options = pickerOptions(
                     actionsBox = TRUE, 
                     size = 10,
                     selectedTextFormat = "count > 3"
                   )
                 ),
                 uiOutput("clusterParams")
               )
             ) |>
               anim_width("100%", "67%")
           )
  ),
  nav_panel("Model Explorer",
            tabName = "explorer",
            layout_sidebar(
              sidebar = sidebar(
                tags$h3("Control Panel"),
                selectInput("data_selection", tags$b("Select Data"), choices = list("Raw Features" = "data", "Scaled Features" = "scaled_data"), selected = "scaled_data"),
                selectInput("dim_red_method", tags$b("Select Dimensionality Reduction Method"), choices = names(dim_red_method_parameters), selected = "PCA"),
                uiOutput("dim_red_params"),
                selectInput("cluster_method", tags$b("Select Clustering Method"), choices = names(cluster_method_parameters)),
                uiOutput("cluster_params"),
                actionButton("run_analysis", "Run Analysis")
              ),
                div(id = "pre_analysis_message", 
                    div(class = "center-message", h4("Please select your parameters and click 'Run Analysis' to start."), style = "height: 400px;")
                ),
                # Placeholders for spinner and results, initially hidden
                hidden(div(id = "spinner_placeholder",
                           br(),
                           tags$h3("Your model is being prepared...", style = "text-align: center;"),
                           plotOutput("plot_placeholder") %>% withSpinner(color = "#0d6efd"))),
                hidden(div(id = "analysis_content",
                           uiOutput("table_results"),
                           br(),
                           uiOutput("plot_context")
                           # plotOutput("plot_results")
                           ))
            )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Dynamic UI for dimension reduction parameters
  output$dimRedParams <- renderUI({
    req(input$dimRedMethod)
    methods <- input$dimRedMethod
    params <- lapply(methods, function(method) {
      switch(method,
             "PCA" = sliderTextInput(
               inputId = "n_components_performance",
               # label = "Components (PCA):", 
               label = tags$span(
                 "Number of Components (PCA): ",
                 popover(
                   bsicons::bs_icon("info-circle"),
                   title = "Number of Principal Components (PCA)",
                   "Determines the number of principal components to retain in the PCA analysis. A higher number of components captures more variance but may include noise, whereas fewer components simplify the model but might miss important variance."
                 )
               ),
               choices = params_values$n_components,
               grid = TRUE,
               width = "85%",
               selected = c(min(params_values$n_components), max(params_values$n_components))
             ),
             "tSNE" = sliderTextInput(
               inputId = "perplexity_performance",
               label = tags$span(
                 "Perplexity (tSNE):", 
                 popover(
                   bsicons::bs_icon("info-circle"),
                   title = "Perplexity (tSNE)",
                   "Controls the balance between local and global aspects of your data in the tSNE model. Higher values consider a broader context, while lower values focus on local neighbors."
                 )
               ),
               choices = params_values$perplexity,
               grid = TRUE,
               width = "85%",
               selected = c(min(params_values$perplexity), max(params_values$perplexity))
             ),
             "Autoencoder" = tagList(
               sliderTextInput(
                 inputId = "encoding_dim_performance",
                 label = tags$span(
                   "Encoding Dimensions (Autoencoder):", 
                   popover(
                     bsicons::bs_icon("info-circle"),
                     title = "Encoding Dimensions (Autoencoder)",
                     "Specifies the size of the encoding layer, which represents the compressed knowledge of the input data. Fewer dimensions force the autoencoder to learn a more compact representation, while more dimensions allow for a more detailed encoding."
                   )
                 ),
                 choices = params_values$encoding_dim,
                 grid = TRUE,
                 width = "85%",
                 selected = c(min(params_values$encoding_dim), max(params_values$encoding_dim))
               ),
               sliderTextInput(
                 inputId = "epochs_performance",
                 label = tags$span(
                   "Epochs (Autoencoder):", 
                   popover(
                     bsicons::bs_icon("info-circle"),
                     title = "Epochs (Autoencoder)",
                     "Defines the number of times the learning algorithm will work through the entire training dataset. More epochs can lead to a better model but increase the risk of overfitting."
                   )
                 ),
                 choices = params_values$epochs,
                 grid = TRUE,
                 width = "85%",
                 selected = c(min(params_values$epochs), max(params_values$epochs))
               )
             ),
             "UMAP" = tagList(
               sliderTextInput(
                 inputId = "n_components_umap_performance",
                 label = tags$span(
                   "Components (UMAP):", 
                   popover(
                     bsicons::bs_icon("info-circle"),
                     title = "Components (UMAP)",
                     "Sets the number of dimensions to which the data should be reduced. Fewer dimensions may result in loss of information, whereas more dimensions can preserve more details of the original data."
                   )
                 ),
                 choices = params_values$n_components,
                 grid = TRUE,
                 width = "85%",
                 selected = c(min(params_values$n_components), max(params_values$n_components))
               ),
               sliderTextInput(
                 inputId = "n_neighbors_performance",
                 label = tags$span(
                   "N Neighbors (UMAP):", 
                   popover(
                     bsicons::bs_icon("info-circle"),
                     title = "Number of Neighbors (UMAP)",
                     "Determines the number of neighboring points used in the UMAP's local manifold approximations. A higher value captures more of the global structure, while a lower value focuses on the local neighborhood."
                   )
                 ),
                 choices = params_values$n_neighbors,
                 grid = TRUE,
                 width = "85%",
                 selected = c(min(params_values$n_neighbors), max(params_values$n_neighbors))
               ),
               sliderTextInput(
                 inputId = "min_dist_performance",
                 label = tags$span(
                   "min dist (UMAP):", 
                   popover(
                     bsicons::bs_icon("info-circle"),
                     title = "Minimum Distance (UMAP)",
                     "Controls the minimum distance apart that points are allowed to be in the low-dimensional representation. Smaller values allow points to cluster more tightly in low-dimensional space."
                   )
                 ),
                 choices = params_values$min_dist,
                 grid = TRUE,
                 width = "85%",
                 selected = c(min(params_values$min_dist), max(params_values$min_dist))
               )
             )
             )
    })
    do.call(tagList, params)
  })
  
  # Dynamic UI for clustering parameters
  output$clusterParams <- renderUI({
    req(input$clusterMethod)
    methods <- input$clusterMethod
    params <- lapply(methods, function(method) {
      switch(method,
             "kmeans" = sliderTextInput(
               inputId = "k_performance",
               label = tags$span(
                 "Number of Clusters (K-Means): ", 
                 popover(
                   bsicons::bs_icon("info-circle"),
                   title = "Number of Clusters (K-Means)",
                   "Specifies the number of clusters to form and centroids to generate. The value of k heavily influences the results."
                 )
               ),
               choices = params_values$k,
               grid = TRUE,
               width = "85%",
               selected = c(min(params_values$k), max(params_values$k))
             ),
             "DBSCAN" = tagList(
               sliderTextInput(
                 inputId = "eps_performance",
                 label = tags$span(
                   "Epsilon (DBSCAN):", 
                   popover(
                     bsicons::bs_icon("info-circle"),
                     title = "Epsilon (DBSCAN)",
                     "Defines the maximum distance between two samples for them to be considered as in the same neighborhood. A smaller value results in a higher density necessary to form a cluster."
                   )
                 ),
                 choices = params_values$eps,
                 grid = TRUE,
                 width = "85%",
                 selected = c(min(params_values$eps), max(params_values$eps))),
               sliderTextInput(
                   inputId = "minPts_performance",
                   label = tags$span(
                     "Minimum Points (DBSCAN)", 
                     popover(
                       bsicons::bs_icon("info-circle"),
                       title = "Minimum Points (DBSCAN)",
                       "The minimum number of points required to form a dense region. Increasing this value demands more points to form a cluster, potentially resulting in more noise points."
                     )
                   ),
                   choices = params_values$minPts,
                   grid = TRUE,
                   width = "85%",
                   selected = c(min(params_values$minPts), max(params_values$minPts))
               )
             ),
             "hclust" = tagList(
               sliderTextInput(
                 inputId = "k_hclust_performance",
                 label = tags$span(
                   "Number of Clusters (Hierarchical):", 
                   popover(
                     bsicons::bs_icon("info-circle"),
                     title = "Number of Clusters (Hierarchical)",
                     "Determines the number of clusters to be formed in hierarchical clustering. The algorithm starts with each point as a separate cluster and merges them until only k clusters remain."
                   )
                 ),
                 choices = params_values$k,
                 grid = TRUE,
                 width = "85%",
                 selected = c(min(params_values$k), max(params_values$k))
               ),
               pickerInput(
                 inputId = "linkage_performance", 
                 label = tags$span(
                   "Linkage Criteria (Hierarchical)", 
                   popover(
                     bsicons::bs_icon("info-circle"),
                     title = "Linkage Criteria (Hierarchical)",
                     "Defines the metric used to measure the distance between sets of observations in hierarchical clustering. Common methods include 'single', 'complete', 'average', and 'Ward's method', each affecting the cluster formation differently."
                   )
                 ),
                 choices = params_values$linkage, 
                 selected = "ward.D2", 
                 multiple = TRUE, 
                 width = "85%",
                 options = list(`actions-box` = TRUE)
               )
             )
      )
    })
    do.call(tagList, params)
  })
  
  heatmap_data <- reactive({
    req(input$dimRedMethod, input$clusterMethod)
    
    selected_methods <- c(input$dimRedMethod, input$clusterMethod)
    all_params <- unlist(lapply(selected_methods, function(method) method_parameter_map[[method]]))
    relevant_params <- unique(all_params) 
    
    performance_data <- performance %>%
      mutate(n_components_umap = n_components, k_hclust = k) %>%
      filter(dim_red_method %in% input$dimRedMethod, cluster_method %in% input$clusterMethod)
    
    if(nrow(performance_data) == 0) return(list(performance_data = data.frame(), height = 0)) # Early return if data is empty
    
    for (param in relevant_params) {
      param_id <- paste(param, "performance", sep = "_")
      if (!is.null(input[[param_id]])) {
        if (length(input[[param_id]]) == 2 && is.numeric(input[[param_id]])) {
          # Numeric range: ensure input[[param_id]] is not empty and has exactly two elements
          performance_data <- performance_data %>%
            filter((!!sym(param) >= input[[param_id]][1] & !!sym(param) <= input[[param_id]][2]) | is.na(!!sym(param)))
        } else {
          # Categorical or single numeric value: ensure input[[param_id]] is not empty
          performance_data <- performance_data %>%
            filter(!!sym(param) %in% input[[param_id]] | is.na(!!sym(param)))
        }
      }
    }
    
    performance_data <- performance_data %>%
      group_by(dim_red_method, cluster_method) %>%
      mutate(has_scores = any(!is.na(silhouette_score)),
             rank_desc = rank(-silhouette_score, ties.method = "first"),  # Rank descending, first for ties
             rank_asc = rank(silhouette_score, ties.method = "first"),    # Rank ascending, first for ties
             max_score = if_else(has_scores, rank_desc == 1, FALSE),       # TRUE for highest score
             min_score = if_else(has_scores, rank_asc == 1, FALSE)) %>%    # TRUE for lowest score
      ungroup() %>%
      select(dim_red_method, cluster_method, ClusterMethod_Parameters, DimRedMethod_Parameters, silhouette_score, has_scores, max_score, min_score)
    # performance_data <- performance_data %>%
    #   group_by(dim_red_method, cluster_method) %>%
    #   mutate(has_scores = any(!is.na(silhouette_score)),
    #          max_score = if_else(has_scores, max(silhouette_score, na.rm = TRUE) == silhouette_score, FALSE),
    #          min_score = if_else(has_scores, min(silhouette_score, na.rm = TRUE) == silhouette_score, FALSE)) %>%
    #   ungroup() %>%
    #   select(dim_red_method, cluster_method, ClusterMethod_Parameters, DimRedMethod_Parameters, silhouette_score, has_scores, max_score, min_score)
    
    num_rows <- length(unique(performance_data$DimRedMethod_Parameters))
    base_height <- 300
    height_per_row <- 10
    total_height <- base_height + (num_rows * height_per_row)
    
    list(data = performance_data, height = total_height)
  })

  output$heatmapPlot <- renderPlot({
    # Ensure heatmap_data() is called at the beginning to use its result throughout
    data_for_plot <- req(heatmap_data())$data 

    # Calculate median silhouette score here to use it in your ggplot call
    median_silhouette <- median(data_for_plot$silhouette_score, na.rm = TRUE)
    
    data_for_plot <- data_for_plot %>%
      mutate(text_color = ifelse(silhouette_score > median_silhouette, "black", "white"))
    
    tile_width = 1 # Adjust as necessary
    tile_height = 1 # Adjust as necessary

    # Now proceed with your ggplot
    p <- ggplot(data_for_plot, aes(x = ClusterMethod_Parameters, y = DimRedMethod_Parameters)) +
      geom_tile(aes(fill = silhouette_score), colour = "white", width = tile_width, height = tile_height) +
      scale_fill_viridis(option = "C", direction = 1, name = "Silhouette Score", na.value = "white") + # Viridis color scale
      geom_tile(data = filter(data_for_plot, max_score), aes(fill = silhouette_score), colour = "green", size = 1, width = tile_width, height = tile_height) + # fill = "gold"
      geom_tile(data = filter(data_for_plot, min_score), aes(fill = silhouette_score), colour = "lightblue", size = 1, width = tile_width, height = tile_height) + # , colour = "white"
      geom_text(aes(label = sprintf("%.2f", silhouette_score), color = text_color), size = 2.5, check_overlap = TRUE) +
      scale_color_identity() +
      facet_grid(dim_red_method ~ cluster_method, scales = "free", space = "free") +
      theme_Publication(base_size = 12) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 6, vjust = 0.5),
            axis.text.y = element_text(size = 6),
            axis.title = element_text(size = 14),
            legend.title = element_text(size = 12),
            legend.text = element_text(size = 10),
            legend.key.height = unit(0.5, 'cm'),
            legend.key.width = unit(2, 'cm')#,
            # strip.background = element_rect(fill = "grey"),
            # strip.text = element_text(size = 9),
            # plot.margin = unit(c(1, 1, 1, 1), "cm")
            ) +
      labs(x = "Clustering Method",
           y = "Dimension Reduction Method",
           title = "Silhouette Scores by Dimension Reduction and Clustering Methods",
           subtitle = "Higher scores indicate better cluster separation, with max/min highlighted")

    # Render the plot with dynamically calculated dimensions
    p
  }, width = function() { "auto" }, height = function() { req(heatmap_data())$height }) 
  
  
  #### RUN MODEL #####
  selected_params <- reactive({
    req(input$dim_red_method, input$cluster_method)
    filtered_params <- best_params %>%
      filter(dim_red_method == input$dim_red_method, cluster_method == input$cluster_method)
    
    if (nrow(filtered_params) == 0) {
      return(list())
    } else {
      return(filtered_params)
    }
  })
  
  reactive_data <- reactive({
    req(input$data_selection)
    
    if (input$data_selection == "data") {
      return(data)
    } else if (input$data_selection == "scaled_data") {
      return(scaled_data)
    }
  })
  
  # Dynamic UI for dimensionality reduction parameters
  output$dim_red_params <- renderUI({
    req(input$dim_red_method)
    params <- selected_params()
    method_params <- dim_red_method_parameters[[input$dim_red_method]]
    
    lapply(method_params, function(param) {
      param_value <- if (!is.null(params[[param]])) params[[param]] else 2  # Default value if param not found
      if (param == "n_components" || param == "perplexity" || param == "encoding_dim" || param == "epochs" || param == "n_neighbors" || param == "min_dist") {
        sliderInput(param, label = dim_red_method_labels[[param]], min = min(params_values[[param]]), max = max(params_values[[param]]), value = param_value)
      }
    })
  })
  
  # Dynamic UI for clustering method parameters
  output$cluster_params <- renderUI({
    req(input$cluster_method)
    params <- selected_params()
    method_params <- cluster_method_parameters[[input$cluster_method]]
    
    lapply(method_params, function(param) {
      param_value <- if (!is.null(params[[param]])) params[[param]] else 2  # Default value if param not found
      if (param == "k" || param == "eps" || param == "minPts") {
        sliderInput(param, label = cluster_method_labels[[param]], min = min(params_values[[param]]), max = max(params_values[[param]]), value = param_value)
      } else if (param == "linkage") {
        selectInput(param, label = cluster_method_labels[[param]], choices = params_values[[param]], selected = param_value)
      }
    })
  })
  
  analysisTrigger <- reactiveVal(0)
  
  # Define reactive expression for results
  observeEvent(input$run_analysis, {
    
    # FOR TESTING:
    # input <- list()
    # input$dim_red_method <- "PCA"
    # input$cluster_method <- "hclust"
    # params <- list(
    #   n_components = 3,
    #   # perplexity = 10,
    #   # encoding_dim = 8,
    #   # epochs = 100,
    #   # n_neighbors = 5,
    #   # min_dist = 0.10,
    #   k = 2,
    #   # eps = 0.1,
    #   # minPts = 4,
    #   linkage = "single"
    # )
    # reactive_data <- scaled_data
    
    analysisTrigger(analysisTrigger() + 1)  # Increment to indicate a new analysis is starting
    shinyjs::hide("analysis_content")
    shinyjs::hide("pre_analysis_message")
    shinyjs::show("spinner_placeholder")
    
    # Collect parameters
    params <- list(
      n_components = input$n_components,
      perplexity = input$perplexity,
      encoding_dim = input$encoding_dim,
      epochs = input$epochs,
      n_neighbors = input$n_neighbors,
      min_dist = input$min_dist,
      k = input$k,
      eps = input$eps,
      minPts = input$minPts,
      linkage = input$linkage
    )
    
    # Call your perform_clustering function here, passing in the selected data and parameters
    # results <- perform_clustering(reactive_data, input$dim_red_method, input$cluster_method, params)
    results <- perform_clustering(reactive_data(), input$dim_red_method, input$cluster_method, params)
    
    # Check if an error was returned
    if (!is.null(results$error) && results$error) {
      # Use showModal for error message display or update UI elements to show the error
      showModal(modalDialog(
        title = "Error",
        paste("The analysis failed with the provided parameters. Please adjust the parameters and try again. \nDetailed error message:", results$message),
        easyClose = TRUE,
        footer = NULL
      ))
      # Optionally, clear any existing output
      output$table_results <- renderUI({})
      output$plot_results <- renderPlot({})
      shinyjs::show("pre_analysis_message")
      shinyjs::hide("spinner_placeholder")
      shinyjs::hide("analysis_content")
      return() # Exit the observer early
    }
    # Check for DBSCAN specific situation where all points are noise
    if (input$cluster_method == "DBSCAN" && !is.null(results$cluster_res) && max(results$cluster_res$cluster) == 0) {
      # Inform the user that all points are classified as noise, suggesting parameter adjustment
      showModal(modalDialog(
        title = "Analysis Results",
        "DBSCAN classified all points as noise. Please adjust the parameters (eps and minPts) and try again.",
        easyClose = TRUE,
        footer = NULL
      ))
      # Optionally, clear or update outputs to reflect this state
      output$table_results <- renderUI({})
      output$plot_results <- renderPlot({})
      shinyjs::show("pre_analysis_message")
      shinyjs::hide("spinner_placeholder")
      shinyjs::hide("analysis_content")
      return() # Exit early
    }
    
    if (input$cluster_method == "kmeans" || input$cluster_method == "DBSCAN") {
      cluster_assignments <- results$cluster_res$cluster
    } else if (input$cluster_method == "hclust") {
      cluster_assignments <- results$cluster_res
    } else {
      cluster_assignments <- NULL
    }
    
    # metrics <- calculate_metrics(results$data_reduced, results$cluster_res)
    metrics <- calculate_cluster_stats(results$data_reduced, cluster_assignments)
    
    output$table_results <- renderUI({
      
      selected_metrics_ids <- c("avg.silwidth", "ch", "dunn", "entropy", "min.cluster.size", "average.within")
      metrics_selected <- as.data.frame(t(metrics)) %>%
        select(all_of(selected_metrics_ids))
      
      kbl_table <-
        kbl(
          metrics_selected,
          format = "html",  # Ensure format is explicitly set to "html"
          escape = FALSE,
          booktabs = TRUE,  # Use booktabs for better line quality
          digits = 2,
          align = "c",
          row.names = FALSE,
          col.names = metrics_names[selected_metrics_ids]
        ) %>%
        kable_styling(bootstrap_options = c("condensed"), full_width = FALSE) %>%
        add_header_above(c(
          "General Clustering Performance" = 3,
          "Cluster Structure Insight" = 3
        ))
      
      # Return HTML table for rendering
      tags$div(
        tags$h3("Cluster Performance Metrics"),
        "To assess the performance of the clustering algorithm with the parameters you have selected, we display a number performance metrics. These metrics can be divided into 'general performance' (looking at separation and cohesion) and 'cluster structure' (distribution of points across clusters and the compactness of clusters). We have selected three common metrics for each of the categories. For the general performance you inspect the ",
        tags$i("Average Silhouette Width"),
        popover(
          bsicons::bs_icon("info-circle"),
          title = "Average Silhouette Width",
          "This metric provides a measure of how close each point in one cluster is to points in the neighboring clusters. Higher silhouette width indicates better separation and cohesion."
        ),
        ", ",
        tags$i("Calinski and Harabasz Index"),
        popover(
          bsicons::bs_icon("info-circle"),
          title = "Calinski and Harabasz Index",
          "This index is a ratio of between-cluster variance to within-cluster variance. Higher values generally indicate clusters are well separated and well defined."
        ),
        ", and the ",
        tags$i("Dunn Index"),
        popover(
          bsicons::bs_icon("info-circle"),
          title = "Dunn Index",
          "Measures the ratio of the smallest distance between observations not in the same cluster to the largest intra-cluster distance. Higher values indicate better clustering by maximizing inter-cluster distances while minimizing intra-cluster distances."
        ),
        ". For the cluster structure, we display the ",
        tags$i("Entropy of Distribution of Cluster Memberships"),
        popover(
          bsicons::bs_icon("info-circle"),
          title = "Entropy of Distribution of Cluster Memberships",
          "Provides a measure of how evenly data points are spread across the clusters. Lower entropy indicates a more definitive classification of points into clusters."
        ),
        ", the ",
        tags$i("Minimum Cluster Size"),
        popover(
          bsicons::bs_icon("info-circle"),
          title = "Minimum Cluster Size",
          "Indicates the size of the smallest cluster. This is useful for identifying if the clustering algorithm is producing any very small clusters, which might be outliers or noise."
        ),
        ", as well as the ",
        tags$i("Average Distance Within Clusters"),
        popover(
          bsicons::bs_icon("info-circle"),
          title = "Average Distance Within Clusters",
          "This metric offers insight into the compactness of the clusters. Lower values indicate that points within a cluster are closer to each other, suggesting better cluster cohesion."
        ),
        ".",
        tags$br(),
        tags$br(),
        HTML(kbl_table)
      )
    })
    
    output$plot_results <- renderPlot({
      if (!is.null(results$data_reduced) && !is.null(results$cluster_res)) {
        
        reduced_data <- as.data.frame(results$data_reduced)
        names(reduced_data) <- c("Dim1", "Dim2")
        reduced_data$Cluster <- as.factor(cluster_assignments)

        p <- ggplot(reduced_data, aes(x = Dim1, y = Dim2)) +
          geom_point(aes(color = Cluster)) + 
          geom_encircle(aes(color = Cluster, fill = Cluster), size = 0.5, expand = 0.08, alpha = 0.3) + 
          scale_color_brewer(palette = "Set1") + 
          scale_fill_brewer(palette = "Set1") + 
          theme_Publication() +
          labs(title = "Cluster Plot", x = "Dimension 1", y = "Dimension 2", fill = "Cluster", color = "Cluster")
        
        print(p)
        
      }
    }, width = "auto", height = 600)
    
    output$plot_context <- renderUI({
      if (!is.null(results$data_reduced) && !is.null(results$cluster_res)) {
        plot_title <- h3("Cluster Plot")
        
        plot_description <- div(
          "To visually assess the resulting clusters, the cluster plot visualizes the clustering results based on the first two dimensions from the dimensionality reduction algorithm. Each point represents an observation, colored by its cluster assignment. Please keep in mind that the clustering may happen in a higher dimensional space than the two that are displayed here. Overlaping cluster shapes might not be overlapping in a higher dimensional space and should be interpreted with caution. Additionally, this visualization depends on the choice of the first two dimensions — when no dimensionality reduction algorithm is chosen, the first two dimensions are merely the first two variables in the feature set and might thus not be meaningful."
        )
        
        plot_output <- plotOutput("plot_results", height = "600px")
        
        # Combine title, description, and plot into a single UI element
        tagList(plot_title, plot_description, plot_output)
      }
    })
    
    shinyjs::hide("spinner_placeholder")
    shinyjs::show("analysis_content")
    
    # Update plot_placeholder to depend on analysisTrigger
    output$plot_placeholder <- renderPlot({
      req(analysisTrigger())  # Force reevaluation whenever analysisTrigger changes
      NULL  # Placeholder content, could be anything that triggers the spinner
    })
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)

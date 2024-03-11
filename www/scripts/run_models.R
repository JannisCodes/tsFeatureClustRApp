library(cluster)  # For silhouette score
library(factoextra) # For other indices
library(fpc)


# AUTOENCODER FUNCTIONS:
create_autoencoder_model <- function(input_dim, encoding_dim) {
  input_layer <- layer_input(shape = input_dim)
  
  # Encoder
  encoded <- input_layer %>%
    layer_dense(units = encoding_dim, activation = 'relu')
  
  # Decoder
  decoded <- encoded %>%
    layer_dense(units = input_dim, activation = 'sigmoid')
  
  # Autoencoder model
  autoencoder <- keras_model(input = input_layer, output = decoded)
  
  # Compile the autoencoder
  autoencoder %>% compile(
    optimizer = 'adam',
    loss = 'binary_crossentropy'
  )
  
  return(autoencoder)
}

autoencoder_get_encoder <- function(autoencoder) {
  encoder_input <- autoencoder$input
  # access the second layer (first hidden layer after input) with integer index
  encoder_output <- autoencoder$get_layer(index = as.integer(1))$output
  
  encoder_model <- keras_model(inputs = encoder_input, outputs = encoder_output)
  
  return(encoder_model)
}



perform_clustering <-
  function(data,
           dim_red_method,
           cluster_method,
           params) {
    # data <- reactive_data
    # dim_red_method <- input$dim_red_method
    # cluster_method <- input$cluster_method
    # params <- params
    
    data_matrix <- as.matrix(data)
    
    # Initialize variables to store results
    data_reduced <- NULL
    cluster_res <- NULL
    
    tryCatch({
      # Apply dimension reduction based on method
      if (dim_red_method == "PCA") {
        if (!is.na(params$n_components)) {
          pca_res <- prcomp(data_matrix, scale. = FALSE)
          if (!is.null(pca_res) && params$n_components <= ncol(pca_res$x)) {
            data_reduced <- predict(pca_res)[, 1:params$n_components]
          } else {
            stop("PCA reduction failed: 'n_components' is invalid or PCA result is NULL.")
          }
        }
      } else if (dim_red_method == "tSNE") {
        if (!is.na(params$perplexity)) {
          set.seed(42)  # For reproducibility
          tsne_res <-
            Rtsne(
              data,
              dims = 2,
              perplexity = params$perplexity,
              pca = FALSE,
              verbose = FALSE
            )
          data_reduced <- tsne_res$Y
        }
      } else if (dim_red_method == "Autoencoder") {
        if (!is.na(params$encoding_dim)) {
          # Ensure your autoencoder model is appropriately defined for `params$encoding_dim`
          autoencoder <-
            create_autoencoder_model(ncol(data_matrix), params$encoding_dim)
          autoencoder %>% fit(
            x = data_matrix,
            y = data_matrix,
            epochs = params$epochs,
            batch_size = 256,
            validation_split = 0.2,
            verbose = 0
          )
          encoder <- autoencoder_get_encoder(autoencoder)
          data_reduced <- predict(encoder, data_matrix)
        }
      } else if (dim_red_method == "UMAP") {
        if (!is.na(params$n_components) &&
            !is.na(params$n_neighbors) && !is.na(params$min_dist)) {
          set.seed(42)  # For reproducibility
          umap_res <-
            umap(
              data,
              n_neighbors = params$n_neighbors,
              min_dist = params$min_dist,
              n_components = params$n_components,
              metric = "euclidean"
            )
          data_reduced <- umap_res
        }
      } else if (dim_red_method == "None") {
        # Skip dimension reduction, use original data
        data_reduced <- data_matrix
      }
      
      if (is.null(data_reduced)) stop("Dimensionality reduction failed.")
      
      # Check if dimension reduction was successful
      if (!is.null(data_reduced)) {
        # Apply clustering based on method
        if (cluster_method == "kmeans") {
          if (!is.na(params$k)) {
            set.seed(42)
            cluster_res <- kmeans(data_reduced, centers = params$k)
          }
        } else if (cluster_method == "DBSCAN") {
          if (!is.na(params$eps) && !is.na(params$minPts)) {
            cluster_res <-
              dbscan::dbscan(data_reduced,
                             eps = params$eps,
                             minPts = params$minPts)
          }
        } else if (cluster_method == "hclust") {
          if (!is.na(params$k) && !is.na(params$linkage)) {
            print(params$k)
            print(params$linkage)
            d <- dist(data_reduced)
            hc <- hclust(d, method = params$linkage)
            cluster_res <- cutree(hc, k = params$k)
          }
        }
      }
      if (is.null(cluster_res)) stop("Clustering failed.")
      
    }, error = function(e) {
      # Return a failure indicator
      return(list(error = TRUE, message = e$message))
    })
    
    # Return list containing reduced data and clustering results, or error message
    return(
      list(
        data_reduced = data_reduced,
        cluster_res = cluster_res,
        error = FALSE,
        message = "Success"
      )
    )
  }


calculate_cluster_stats <- function(data_reduced, cluster_assignments) {
  # Calculate the dissimilarity matrix
  d <- dist(data_reduced)
  
  # Initialize a list to store performance metrics
  clustering_performance <- list()
  
  # Calculate metrics using cluster.stats
  cluster_stats <- cluster.stats(
    d = d,
    clustering = cluster_assignments,
    G2 = TRUE,
    G3 = TRUE,
    aggregateonly = TRUE
  )
  
  # Convert results to a named list for easier interpretation
  metrics <- unlist(cluster_stats, use.names = TRUE)
  
  return(metrics)
}

metrics_names <- c(
  "n" = "number of cases", 
  "cluster.number" = "number of points", 
  "min.cluster.size" = "size of smallest cluster", 
  "noisen" = "number of noise points", # Not relevant for kmeans 
  "average.between" = "average distance between clusters", 
  "average.within" = "average distance within clusters", # (reweighted so that every observation, rather than every distance, has the same weight)
  "max.diameter" = "maximum cluster diameter",
  "min.separation" = "minimum cluster separation", 
  "ave.within.cluster.ss" = "within clusters sum of squares", # generalisation
  "avg.silwidth" = "average silhouette width", 
  "g2" = "Goodman Kruskal's Gamma coefficient", # See Milligan and Cooper (1985), Gordon (1999, p. 62). 
  "g3" = "G3 coefficient", # See Gordon (1999, p. 62)
  "pearsongamma" = "Normalized gamma", # correlation between distances and a 0-1-vector where 0 means same cluster, 1 means different clusters. see Halkidi et al. (2001)
  "dunn" = "Dunn index", # minimum separation / maximum diameter, Dunn index, see Halkidi et al. (2002).
  "dunn2" = "Dunn index 2", # minimum average dissimilarity between two cluster / maximum average within cluster dissimilarity, another version of the family of Dunn indexes
  "entropy" = "entropy of distribution of cluster memberships", # see Meila(2007)
  "wb.ratio" = "average within / average between", 
  "ch" = "Calinski and Harabasz index", # (Calinski and Harabasz 1974, optimal in Milligan and Cooper 1985; generalised for dissimilarites in Hennig and Liao 2013)
  "widestgap" = "widest within-cluster gap", 
  "sindex" = "separation index"
)

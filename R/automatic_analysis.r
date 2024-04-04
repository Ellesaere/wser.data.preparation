automatic_analysis <- function(dataset, dependent_vars, excluded_cols = NULL) {
    results_list <- list()
    
    for (j in seq_along(dependent_vars)) {
        dependent_var <- dependent_vars[j]
        
        # Construct the model formula excluding the specified columns
        exclude_columns <- c(dependent_var, excluded_cols)
        model_formula <- as.formula(paste(dependent_var, "~", paste(names(dataset)[!names(dataset) %in% exclude_columns], collapse = " + ")))
        
        # Regression Results
        model <- lm(model_formula, data = dataset)
        summary(model)
        
        # Lasso/Ridge/Elastic Net
        x <- model.matrix(model)
        y <- model$model[[dependent_var]]
        
        lasso_model <- cv.glmnet(x, y, alpha = 1, family = "gaussian")
        lasso_coefficients_A <- coef(lasso_model, s = "lambda.min")
        lasso_coefficients_B <- coef(lasso_model, s = "lambda.1se")
        
        ridge_model <- cv.glmnet(x, y, alpha = 0, family = "gaussian", type.measure = "mse")
        ridge_coefficients <- coef(ridge_model, s = "lambda.min")
        
        enet_model <- cv.glmnet(x, y, alpha = 0.5, family = "gaussian", type.measure = "mse")
        enet_coefficients <- coef(enet_model, s = "lambda.min")
        
        # Correlations
        the_data_for_corr <- copy(dataset)
        
        zero_sd_vars <- which(sapply(the_data_for_corr, sd) == 0)
        
        if (length(zero_sd_vars) > 0) {
            the_data_for_corr <- the_data_for_corr[, -zero_sd_vars]
        }
        
        cor_matrix <- cor(the_data_for_corr, use = "pairwise.complete.obs")
        corrplot(cor_matrix, method = "color")
        highly_correlated <- caret::findCorrelation(cor_matrix, cutoff = 0.8)
        the_data_for_corr <- subset(the_data_for_corr, select = -highly_correlated)
        
        # Scatterplots
        # You can add scatterplot or other visualization code here if needed
        
        # VIF
        lm_out <- lm(formula(paste(dependent_var, "~ .")), data = the_data_for_corr)
        vif_values <- vif(lm_out)
        high_vif_threshold <- 10
        moderate_vif_threshold <- 5
        high_vif_variables <- names(vif_values[vif_values > high_vif_threshold])
        moderate_vif_variables <- names(vif_values[vif_values > moderate_vif_threshold & vif_values <= high_vif_threshold])
        acceptable_vif_variables <- names(vif_values[vif_values <= moderate_vif_threshold])
        
        # Dimensionality Reduction (PCA)
        pca <- prcomp(the_data_for_corr, scale. = TRUE)
        summary(pca)
        
        # Variable Importance (Random Forests)
        rf_model <- randomForest(formula(paste(dependent_var, "~ .")), data = the_data_for_corr, ntree = 100)
        var_importance_rf <- importance(rf_model)
        var_importance_rf <- as.data.frame(var_importance_rf)
        var_importance_rf <- rownames_to_column(var_importance_rf, var = "Variable")
        
        # Sort the var_importance_rf dataframe by decreasing order of importance
        var_importance_rf <- var_importance_rf[order(var_importance_rf$IncNodePurity, decreasing = TRUE), ]
        
        mean_incnodepurity <- mean(var_importance_rf$IncNodePurity)
        threshold <- mean_incnodepurity
        important_vars_rf <- var_importance_rf[var_importance_rf$IncNodePurity > threshold, ]
        
        # Variable Importance (GBM)
        gbm_model <- gbm(formula(paste(dependent_var, "~ .")), data = the_data_for_corr, n.trees = 100, interaction.depth = 3)
        var_importance_gbm <- summary(gbm_model)
        
        # Store the results in a list along with the dependent variable used
        results_list[[length(results_list) + 1]] <- list(
            dependent_var = dependent_var,
            lm_summary = summary(model),
            lasso_coefficients_A = lasso_coefficients_A,
            lasso_coefficients_B = lasso_coefficients_B,
            ridge_coefficients = ridge_coefficients,
            enet_coefficients = enet_coefficients,
            highly_correlated_vars = highly_correlated,
            high_vif_vars = high_vif_variables,
            moderate_vif_vars = moderate_vif_variables,
            acceptable_vif_vars = acceptable_vif_variables,
            pca_summary = summary(pca),
            important_vars_rf = important_vars_rf,
            var_importance_gbm = var_importance_gbm
        )
        
    }
    
    # Return the list of results
    return(results_list)
}

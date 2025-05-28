for (k_val in 1:100) {
  model <- train(
    pp_knn,
    data = daily_fluxes_knn_train |> drop_na(),
    method = "knn",
    trControl = trainControl(method = "none"),
    tuneGrid = data.frame(k = k_val),
    metric = "MAE"
  )
  
  preds <- predict(model, newdata = daily_fluxes_knn_test |> drop_na())
  obs <- daily_fluxes_knn_test |> drop_na() |> pull(GPP_NT_VUT_REF)
  
  mae_val <- MAE(obs, preds)
  
  results_test <- results_test |> 
    add_row(k = k_val, MAE = mae_val)
  
  # Early stopping check
  if (mae_val < best_mae) {
    best_mae <- mae_val
    best_k <- k_val
    counter <- 0  # reset counter on improvement
  } else {
    counter <- counter + 1
  }
  
  if (counter >= patience) {
    print(paste("Stop at k =", k_val, ", best k =", best_k, ", MAE = ", round(best_mae, 4)))
    break
  }
}
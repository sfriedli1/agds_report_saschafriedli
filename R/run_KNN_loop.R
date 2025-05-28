for (k_val in 1:100) {
  model <- train(
    pp_knn,
    data = daily_fluxes_knn_train |> drop_na(),
    method = "knn",
    trControl = trainControl(method = "none"),
    tuneGrid = data.frame(k = k_val),
    metric = "MAE"
  )
  
  preds_train <- predict(model, newdata = daily_fluxes_knn_train |> drop_na())
  obs_train <- daily_fluxes_knn_train |> drop_na() |> pull(GPP_NT_VUT_REF)
  
  mae_val_train <- MAE(obs_train, preds_train)
  
  results_both <- results_both |> 
    add_row(type = "train", k = k_val, MAE = mae_val_train)
  
  preds_test <- predict(model, newdata = daily_fluxes_knn_test |> drop_na())
  obs_test <- daily_fluxes_knn_test |> drop_na() |> pull(GPP_NT_VUT_REF)
  
  mae_val_test <- MAE(obs_test, preds_test)
  
  results_both <- results_both |> 
    add_row(type = "test", k = k_val, MAE = mae_val_test)
  
}
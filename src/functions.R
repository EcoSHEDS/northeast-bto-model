load_config <- function() {
  config <- config::get()
  config$version <- config::get(file = "version.yml")
  config$wd <- file.path(config$root, config$version$bto)

  if(!dir.exists(config$wd)) {
    stop(glue::glue("Working directory for this model version ({config$version$bto}) does not exist, please create it within the root directory ({config$root})"))
  }

  config
}

db_connect <- function() {
  config <- load_config()
  DBI::dbConnect(
    RPostgreSQL::PostgreSQL(),
    dbname = config$db$dbname,
    host = config$db$host,
    port = config$db$port,
    user = config$db$user,
    password = config$db$password
  )
}

model_pred <- function(m, df) {
  # m = fitted model
  # df = input data frame

  y_pred <- inv.logit(predict(m, df, allow.new.levels = TRUE))
  y_obs <- df$presence

  pred_roc <- roc(y_pred, as.factor(y_obs))
  df_roc <- tibble(
    cutoff = pred_roc[[1]],
    fpr = pred_roc[[2]],
    tpr = pred_roc[[3]]
  )

  pred_stats <- list(
    n = length(y_obs),
    sens = auc(AUC::sensitivity(y_pred, as.factor(y_obs))),
    spec = auc(AUC::specificity(y_pred, as.factor(y_obs))),
    acc = auc(AUC::accuracy(y_pred, as.factor(y_obs))),
    auc = auc(AUC::roc(y_pred, as.factor(y_obs))),
    err = mean((y_pred > 0.5 & y_obs == 0) | (y_pred <= 0.5 & y_obs == 1)),
    fpr = mean(y_pred > 0.5 & y_obs == 0),
    fnr = mean(y_pred <= 0.5 & y_obs == 1)
  )

  list(
    y_pred = y_pred,
    y_obs = y_obs,
    roc = df_roc,
    stats = pred_stats
  )
}

plot_auc <- function(y_pred, y_obs) {
  x_sens <- AUC::sensitivity(y_pred, as.factor(y_obs))
  x_spec <- AUC::specificity(y_pred, as.factor(y_obs))
  x_acc <- AUC::accuracy(y_pred, as.factor(y_obs))
  x_roc <- AUC::roc(y_pred, as.factor(y_obs))

  p_sens <- tibble(
    cutoff = x_sens$cutoffs,
    value = x_sens$measure
  ) %>%
    ggplot(aes(cutoff, value)) +
    geom_line() +
    geom_label(
      data = tibble(auc = auc(x_sens)),
      aes(x = 0.01, y = 0.01, label = paste0("Sensitivity = ", sprintf("%.2f", auc))),
      hjust = 0, vjust = 0
    ) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 6), limits = c(0, 1)) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 6), limits = c(0, 1)) +
    labs(
      x = "Cutoff",
      y = "Sensitivity",
      title = "Sensitivity",
      subtitle = "True Positive Rate"
    ) +
    theme(aspect.ratio = 1)

  p_spec <- tibble(
    cutoff = x_spec$cutoffs,
    value = x_spec$measure
  ) %>%
    ggplot(aes(cutoff, value)) +
    geom_line() +
    geom_label(
      data = tibble(auc = auc(x_spec)),
      aes(x = 0.99, y = 0.01, label = paste0("Specificity = ", sprintf("%.2f", auc))),
      hjust = 1, vjust = 0
    ) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 6), limits = c(0, 1)) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 6), limits = c(0, 1)) +
    labs(
      x = "Cutoff",
      y = "Specificity",
      title = "Specificity",
      subtitle = "True Negative Rate"
    ) +
    theme(aspect.ratio = 1)

  p_acc <- tibble(
    cutoff = x_acc$cutoffs,
    value = x_acc$measure
  ) %>%
    ggplot(aes(cutoff, value)) +
    geom_line() +
    geom_label(
      data = tibble(auc = auc(x_acc)),
      aes(x = 0.01, y = 0.01, label = paste0("Accuracy = ", sprintf("%.2f", auc))),
      hjust = 0, vjust = 0
    ) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 6), limits = c(0, 1)) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 6), limits = c(0, 1)) +
    labs(
      x = "Cutoff",
      y = "Accuracy",
      title = "Accuracy",
      subtitle = "True Positive and Negative Rate"
    ) +
    theme(aspect.ratio = 1)

  p_roc <- tibble(
    fpr = x_roc$fpr,
    tpr = x_roc$tpr
  ) %>%
    ggplot(aes(fpr, tpr)) +
    geom_abline(color = "gray80") +
    geom_line() +
    geom_label(
      data = tibble(auc = auc(x_roc)),
      aes(x = 0.99, y = 0.01, label = paste0("AUC = ", sprintf("%.2f", auc))),
      hjust = 1, vjust = 0
    ) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 6), limits = c(0, 1)) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 6), limits = c(0, 1)) +
    labs(
      x = "1 - Specificity",
      y = "Sensitivity",
      title = "ROC Curve",
      subtitle = " "
    ) +
    theme(aspect.ratio = 1)

  grid.arrange(p_sens, p_spec, p_acc, p_roc, ncol = 2)
}
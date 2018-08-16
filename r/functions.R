load_config <- function(path = "../") {
  # path: path to root of repo (where config.sh and version.sh are located)
  readRenviron(file.path(path, "config.sh"))
  readRenviron(file.path(path, "version.sh"))

  wd <- file.path(Sys.getenv("SHEDS_BTO_ROOT"), Sys.getenv("SHEDS_BTO_VERSION"))

  if (!file.exists(wd)) {
    stop(paste0("ERROR: could not find working directory (", wd, ")"))
  }

  list(
    version = Sys.getenv("SHEDS_BTO_VERSION"),
    root = Sys.getenv("SHEDS_BTO_ROOT"),
    wd = wd,
    db = list(
      dbname = Sys.getenv("SHEDS_BTO_DB_DBNAME"),
      host = Sys.getenv("SHEDS_BTO_DB_HOST"),
      password = Sys.getenv("SHEDS_BTO_DB_PASSWORD"),
      port = Sys.getenv("SHEDS_BTO_DB_PORT"),
      user = Sys.getenv("SHEDS_BTO_DB_USER")
    ),
    temp = list(
      path = Sys.getenv("SHEDS_BTO_TEMP_PATH")
    )
  )
}

model_pred <- function(m, df) {
  # m = fitted model
  # df = input data frame

  y_pred <- inv.logit(predict(m, df, allow.new.levels = TRUE))
  y_obs <- df$presence

  pred_roc <- roc(y_pred, as.factor(y_obs))
  df_roc <- data_frame(
    cutoff = pred_roc[[1]],
    fpr = pred_roc[[2]],
    tpr = pred_roc[[3]]
  )

  pred_stats <- list(
    sens = auc(sensitivity(y_pred, as.factor(y_obs))),
    spec = auc(specificity(y_pred, as.factor(y_obs))),
    acc = auc(accuracy(y_pred, as.factor(y_obs))),
    auc = auc(roc(y_pred, as.factor(y_obs))),
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


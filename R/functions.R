db_connect <- function() {
  dotenv::load_dot_env()
  DBI::dbConnect(
    RPostgres::Postgres(),
    host = Sys.getenv("BTO_DB_HOST"),
    port = Sys.getenv("BTO_DB_PORT"),
    dbname = Sys.getenv("BTO_DB_DBNAME"),
    user = Sys.getenv("BTO_DB_USER"),
    password = Sys.getenv("BTO_DB_PASSWORD")
  )
}

create_model_pred <- function (x, m) {
  x |>
    mutate(
      pred = boot::inv.logit(predict(m, x, allow.new.levels = TRUE))
    )
}

create_model_gof <- function (x) {
  x |>
    transmute(
      partition, featureid, pred, presence,
      result = case_when(
        presence == 0 & pred < 0.5 ~ "TN",
        presence == 0 & pred >= 0.5 ~ "FP",
        presence == 1 & pred < 0.5 ~ "FN",
        TRUE ~ "TP"
      )
    ) |>
    nest_by(partition) |>
    mutate(
      roc = list({
        r <- AUC::roc(data$pred, as.factor(data$presence))
        tibble(
          cutoff = r[[1]],
          fpr = r[[2]],
          tpr = r[[3]]
        )
      }),
      cm = list({
        caret::confusionMatrix(
          data = factor(1 * (data$pred > 0.5)),
          reference = factor(data$presence),
          mode = "sens_spec",
          positive = "1"
        )
      }),
      stats = list({
        tibble(
          n = nrow(data),
          TP = sum(data$result == "TP"),
          TN = sum(data$result == "TN"),
          FP = sum(data$result == "FP"),
          FN = sum(data$result == "FN"),
          auc = AUC::auc(AUC::roc(data$pred, as.factor(data$presence)))
        )
      })
    ) |>
    unnest(stats)
}

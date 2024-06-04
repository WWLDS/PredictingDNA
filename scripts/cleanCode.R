###############################################
##                                           ##
## Wrightington, Wigan & Leigh DNA predictor ##
## https://github.com/WWLDS/dna              ##
##                                           ##
###############################################


# please see readMe for full information.

# note that the associated dataset and this script are in a significantly 
# reduced form to maximise patient anonymity and shareability. Please contact
# thomas.ingram@wwl.nhs.uk for access.


### ----------------------------------------------------------------------------
# check if the package librarian installed then load Librarian (Librarian is a
# package installation and management package that merges CRAN, GitHub and
# Bioconductor

if (!require(librarian)) {
  install.packages("librarian")
  library(librarian)
}

# install and load libraries
librarian::shelf(
  tidyverse, here, caret,
  pROC, ROCR, rsample, tidymodels,
  pander, xgboost, glue,
  data.table, digest, lintr, styler
)


### ----------------------------------------------------------------------------
# import example dataset
data <- read_csv(here("rawData/dnaData.csv"))


### ----------------------------------------------------------------------------
# data engineering
# calculate dataset mean dna proportion for mean imputation later
overallRate <- data |>
  group_by(dnaFlag) |>
  summarise(count = n()) |>
  ungroup() |>
  mutate(prop = count / sum(count))
rate <- filter(overallRate, dnaFlag == 1)$prop


# calculate dataset cancellation proportion for mean imputation later
overallRateCancel <- data |>
  group_by(cancelFlag) |>
  summarise(count = n()) |>
  ungroup() |>
  mutate(prop = count / sum(count))
cancelRate <- filter(overallRateCancel, cancelFlag == 1)$prop


# create clinic dna rates var
clinicNum <- data |>
  # identify small sample clinics and group together
  group_by(anonClinic, .drop = F) |>
  summarise(count = n()) |>
  mutate(anonClinic = if_else(count < 500, "Other", anonClinic)) |>
  group_by(anonClinic, .drop = F) |>
  summarise(across(count, sum)) |>
  pull(anonClinic)


clinicTest <- data |>
  # filter out small sample clinics then calculate clinic dna rates
  filter(anonClinic %in% clinicNum) |>
  group_by(anonClinic, dnaFlag, .drop = F) |>
  summarise(count = n()) |>
  group_by(anonClinic, dnaFlag, .drop = F) |>
  summarise(across(count, sum)) |>
  mutate(clinicProp = count / sum(count)) |>
  filter(dnaFlag == 1) |>
  select(-count, -dnaFlag)


# create multiple appointments in single day var
multiDay <- data |>
  group_by(anonID, appointment_dt) |>
  count() |>
  mutate(multiDay = if_else(n > 1, 1, 0))


# create personal dna likelihood var using nth order Markov chain
markovEvent <- c("dnaFlag", "cancelFlag")
markovFun <- function(data, event) {
  dataMarkov <- data |>
    # select only the variables required
    select(
      event,
      appointment_dt,
      anonID
    )

  mChain <- dataMarkov |>
    # sort appts in date order of occurrence for each patient
    arrange(anonID, desc(appointment_dt)) |>
    group_by(anonID) |>
    mutate(id = row_number()) |>
    # maximum of 6th order Markov Chain
    filter(id <= 6) |>
    select(-appointment_dt) |>
    pivot_wider(values_from = event, names_from = id) |>
    unite(col = "sequence", 2:7, sep = "", na.rm = T)

  # count number of times each attendance sequence is present in the dataset
  chainProb <- mChain |>
    group_by(sequence) |>
    summarise(count = n())

  # filter out last appointment and create their attendance sequence
  seqCreate <- dataMarkov |>
    group_by(anonID) |>
    dplyr::filter(appointment_dt != max(appointment_dt)) |>
    ungroup() |>
    arrange(anonID, desc(appointment_dt)) |>
    group_by(anonID) |>
    mutate(id = row_number()) |>
    filter(id <= 5) |>
    select(-appointment_dt) |>
    pivot_wider(values_from = event, names_from = id) |>
    unite(col = "sequence", 2:6, sep = "", na.rm = T) |>
    ungroup()

  # for each patient calc probability they will dna on their next appointment
  onlySeq <- unique(seqCreate$sequence)
  mChainList <- list()

  for (i in onlySeq) {
    x1 <- as.character(glue(1, "{i}", sep = ""))
    x0 <- as.character(glue(0, "{i}", sep = ""))
    probX1 <- (filter(chainProb, sequence == x1)$count)
    probX0 <- (filter(chainProb, sequence == x0)$count)

    if (length(probX1 | probX0) > 0) {
      prob <- probX1 / (probX0 + probX1)
    } else {
      x1 <- gsub(".{1}$", "", x1)
      x0 <- gsub(".{1}$", "", x0)
      probX1 <- (filter(chainProb, sequence == x1)$count)
      probX0 <- (filter(chainProb, sequence == x0)$count)

      prob <- probX1 / (probX0 + probX1)
    }

    df <- tibble(
      event = prob,
      sequence = i,
    )
    mChainList[[paste0(i, "_")]] <- df
    chainKeys <- bind_rows(mChainList)

    noHistX1 <- (filter(chainProb, sequence == 1)$count)
    noHistX0 <- (filter(chainProb, sequence == 0)$count)
    noHist <- noHistX1 / (noHistX0 + noHistX1)
    finalBind <- c(noHist, NA)

    seqProbs <- rbind(chainKeys, finalBind)
  }
  rateJoin <- left_join(seqCreate, seqProbs, by = "sequence")
  return(rateJoin)
}


# join markov chain DNA and cancel rates to master dataframe
markovData <- data |>
  left_join(markovFun(data, markovEvent[1]), by = "anonID") |>
  mutate(dnaMarkov = if_else(is.na(event), rate, event)) |>
  select(-c(sequence, event)) |>
  left_join(markovFun(data, markovEvent[2]), by = "anonID") |>
  mutate(cancelMarkov = if_else(is.na(event), cancelRate, event)) |>
  select(-c(sequence, event))


# create three month count of appointments var
threeMoCount <- markovData |>
  filter(appointment_dt > max(date(appointment_dt)) - 90) |>
  group_by(anonID) |>
  summarise(threeMoCount = n())


# join all other created features to master dataframe
varData <- markovData |>
  # join clinic dna rate column to main dataset
  left_join(clinicTest,
    by = c("anonClinic")
  ) |>
  # mean impute dataset dna rate for small clinics
  mutate(clinicProp = if_else(is.na(clinicProp), rate, clinicProp)) |>
  left_join(threeMoCount,
    by =
      c("anonID")
  ) |>
  # if no appointment in last 3 months then 0
  mutate(threeMoCount = if_else(is.na(threeMoCount), 0, threeMoCount)) |>
  left_join(multiDay, by = c("anonID", "appointment_dt")) |>
  select(-n)


# single row per patient to ensure independence
indData <- varData |>
  group_by(anonID) |>
  dplyr::slice(which.max(appointment_dt)) |>
  ungroup()


# dataframe ready for ml work. All vars numeric and named appropriately
mlReady <- indData |>
  select(-c(
    anonID,
    anonClinic,
    date_letter_received_dt, appointment_dt,
    cancelFlag
  )) |>
  rename(
    "DNA probability by sequence" = dnaMarkov,
    "Cancellation probability by sequence" = cancelMarkov,
    "Days between booking and appointment" = time_to_be_seen_days,
    "Home to clinic distance (km)" = travelDist,
    "Hospital" = hospital,
    "Age" = age,
    "Deprivation decile" = depriv_decile,
    "Attendance weekday" = day_of_week,
    "Appointment type" = appointment_type,
    "Referral priority" = referral_priority,
    "Attendance hour of day" = attendance_time,
    "Month" = month,
    "Suspected cancer diagnosis" = suspected_cancer,
    "DNA probability by clinic" = clinicProp,
    "Source of referral" = refSource,
    "Three month appt count" = threeMoCount,
    "Female specialty" = femaleSpec,
    "Paediatric specialty" = paedSpec,
    "New appt" = newAppt,
    "Physiotherapy" = physioSpec,
    "RAEI, Leigh or Thomas Linacre" = raeiLeighTl,
    "Trauma & Orthopaedics appt" = `T&Oappt`,
    "Follow-up appt" = followUpAppt,
    "Multiple appts on same day" = multiDay
  )


# remove additional vars shown to increase error rate through trialling
mlFilter <- mlReady |>
  select(-c(
    "Appointment type",
    "Month", "Referral priority",
    "Hospital"
  ))
colSums(is.na(mlFilter))


# ------------------------------------------------------------------------------
# machine learning section
set.seed(NULL)
listPerf <- list()
# train model 25 times and take mean performance metrics
y <- 1
while (y < 26) {
  # convert Class to factor for under-sampling
  data <- mlFilter |>
    mutate(
      dnaFlag = as_factor(if_else(dnaFlag == 1, 1, 0)),
      id = row_number()
    )

  # create train-test split
  trainSample <- data |>
    group_by(dnaFlag) |>
    sample_frac(.75)

  testSet <- data |>
    anti_join(trainSample, by = "id") |>
    mutate(Class = as.numeric(dnaFlag)) |>
    mutate(Class = if_else(dnaFlag == 1, 1, 0)) |>
    select(-c(dnaFlag, id))

  # under-sampling majority group to maximise recall
  dSample <- downSample(
    trainSample,
    trainSample$dnaFlag
  )

  # convert Class to numeric for xgboost
  trainSet <- dSample |>
    mutate(Class = as.numeric(Class)) |>
    mutate(Class = if_else(dnaFlag == 1, 1, 0)) |>
    select(-c(dnaFlag, id))

  # select features for xgboost
  trainData <- subset(trainSet, select = -c(Class))
  trainLabel <- subset(trainSet, select = c(Class))
  testData <- subset(testSet, select = -c(Class))
  testLabel <- subset(testSet, select = c(Class))

  # create xgb required matrix
  dTrain <- xgb.DMatrix(as.matrix(trainData), label = trainLabel$Class)
  dTest <- xgb.DMatrix(as.matrix(testData), label = testLabel$Class)

  # xgb 5-fold cross validation
  cv <- xgb.cv(dTrain,
    params = list(
      objective = "binary:logistic"
    ),
    nfold = 5,
    nrounds = 150,
    print_every_n = 50,
    metrics = "auc",
    prediction = T
  )
  evalframe <- as.data.frame(cv$evaluation_log)
  head(evalframe)
  (nRounds <- which.max(evalframe$test_auc_mean))

  # xgb hyperparameter tuning
  params <- list(
    objective = "binary:logistic",
    eval_metric = "aucpr",
    eval_metric = "auc",
    eta = 0.1,
    booster = "gbtree",
    gamma = 0,
    max_depth = 6,
    min_child_weight = 1,
    subsample = 1,
    colsample_bytree = 1
  )

  # xgboost model run
  xgbFit <- xgb.train(params, dTrain,
    nrounds = nRounds,
    watchlist = list(train = dTrain, val = dTest),
    print_every_n = 10,
    early_stopping_rounds = 10,
    maximise = F
  )

  pred <- as.numeric(predict(xgbFit, dTest) >= 0.5)
  obs <- getinfo(dTest, "label")
  cm <- confusionMatrix(table(pred, obs),
    positive = "1", mode = "everything"
  )
  metrics <- data.frame(t(cm$byClass),
    date = today()
  )
  # save model runs and bind together to calculate mean performance
  write_csv(metrics, here(glue("rawData/trainingResults/{y}.csv")))

  listPerf[[paste0(y)]] <- metrics
  allPerf <- bind_rows(listPerf)

  print(y)
  y <- y + 1
}


# mean performance output of model runs
allPerf <- subset(allPerf, select = -c(date))
perfMean <- colMeans(allPerf)
perfMean


# confusion matrix of single run (run 25)
pred <- as.numeric(predict(xgbFit, dTest) >= 0.5)
obs <- getinfo(dTest, "label")
cm <- confusionMatrix(table(pred, obs),
  positive = "1", mode = "everything"
)
metrics <- data.frame(t(cm$byClass),
  date = today()
)
cm


# SHAP plot output
x <- as.matrix(trainData)
shapPlot <- xgb.ggplot.shap.summary(x,
  model = xgbFit, target_class = 1,
  top_n = 25
) +
  theme_ipsum(
    axis_title_just = "cc",
    axis_title_face = "bold",
    axis_text_size = 30,
    axis_title_size = 30
  ) +
  labs(
    x = "",
    y = "SHAP score",
    colour = ""
  ) +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 30),
    legend.key.height = unit(3, "cm"),
    legend.text = element_text(size = 36),
    plot.caption = element_text(size = 18),
    plot.title = element_text(size = 30),
    axis.text.x = element_text(size = 30, angle = 0)
  )
shapPlot

do_job0 <- function(
  file,
  ExternalVariables = NULL,
  jobName = NULL,
  saveTo = "qs2",
  checkMem = F,
  maxMemory = NULL,
  performSaving = T,
  fetchSessionInfo = T,
  activeGC = 0
) {
  if (!is.numeric(activeGC)) {
    activeGC <- 0
  }
  if (
    (!is.null(maxMemory)) &&
      is.integer(maxMemory)
  ) {
    memoryLimit <- maxMemory
  } else {
    memoryLimit <- NULL
  }

  globe_stamp <- running_time_stamp(self_name = jobName)
  createPath(globe_stamp)
  lockFile(globe_stamp)
  cat2 <- function(x) {
    messageTracing(x, globe_stamp)
  }
  clearMessage(globe_stamp)
  cat2("---BackgroundJobR Session---\n")
  cat2(paste0("Session ID: ", globe_stamp, "\n"))
  exprs <- parse(file, keep.source = TRUE)
  exprsText <- sapply(exprs, function(e) {
    paste(deparse(e), collapse = " ")
  })
  evaluateEnv <- new.env()
  if (
    is.list(ExternalVariables) &
      !is.data.frame(ExternalVariables)
  ) {
    list2env(ExternalVariables, envir = evaluateEnv)
    cat2(paste0(
      "Successful assigned vars: ",
      paste(ls(envir = evaluateEnv), collapse = ", "),
      "\n"
    ))
  }
  noErrorFlag <- T
  errorLine <- NULL
  TOTALlines <- length(exprs)
  for (i in 1:length(exprs)) {
    if (activeGC >= 0) {
      if (identical(i %% activeGC, 0)) {
        gc()
      }
    }
    if (noErrorFlag) {
      tryCatch(
        {
          cat2(paste0(
            "running:(",
            i,
            "/",
            TOTALlines,
            ") ",
            exprsText[i],
            "\n"
          ))
          eval(exprs[i], envir = evaluateEnv)
          cat2(paste0("done:", exprsText[i], "\n"))

          # -- memory handling --
          if (checkMem) {
            eval(wild_lobstr, envir = evaluateEnv)
            memused <- get(".__.bgjob_lobster_memusing.", envir = evaluateEnv)
            memused <- as.numeric(memused) / 1000 / 1000
            memused <- round(memused, 3)
            cat2(paste0("Memory used by current session:", memused, " MB \n"))
            if (!is.null(memoryLimit)) {
              if (memused > memoryLimit) {
                stop("Mermory limit OUT!")
              }
            }
          }
        },
        error = function(e) {
          cat2(paste0("Error at Step ", i, "\n"))
          noErrorFlag <<- F
          errorLine <<- i
          msg <- conditionMessage(e)
          cat2(paste0("Error Message:", msg, "\n"))
        }
      )
    }
    log_messages <- get(globe_stamp, envir = error_message_env)
    writeLines(log_messages, con = paste0(globe_stamp, "/run_history.log"))
  }
  if (identical(noErrorFlag, F) && performSaving) {
    objs <- mget(ls(envir = evaluateEnv), envir = evaluateEnv)
    obj_save_path00 <- paste0(globe_stamp, "/objs")
    bgSave(objs, obj_save_path00, saveTo = saveTo)
    cat2(paste0("On error environment saved to: ", obj_save_path00, "\n"))
  }
  if (identical(noErrorFlag, T) && fetchSessionInfo) {
    eval(sessioninfohook, envir = evaluateEnv)
    ssinfo <- get(".__.bgjob_sessionInfo.", envir = evaluateEnv)
    cat2(paste0("Session Info: \n", paste(ssinfo, collapse = " \n"), " \n"))
  }
  try({
    removeFile(paste0(globe_stamp, "/.backgroundjobR.lockFile"))
  })
  rm(list = ls(envir = evaluateEnv), envir = evaluateEnv)
  gc()
  log_messages_final <- get(globe_stamp, envir = error_message_env)
  writeLines(log_messages_final, con = paste0(globe_stamp, "/run_history.log"))
  invisible(log_messages_final)
}

createPath <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }
}

lockFile <- function(stamp) {
  if (file.exists(paste0(stamp, "/.backgroundjobR.lockFile"))) {
    stop(
      "Already running.
         If you believe this is a mistake,
         please remove `.backgroundjobR.lockFile` in the target dictionary. "
    )
  } else {
    if (!file.create(paste0(stamp, "/.backgroundjobR.lockFile"))) {
      stop("Can't write into target dictionary. ")
    }
  }
}

removeFile <- function(file_path) {
  if (file.exists(file_path)) {
    if (file.remove(file_path)) {} else {
      warning("Can't remove file. There might be some error")
    }
  } else {
    message("Can't find file. There might be some error")
  }
}

clearMessage <- function(stamp) {
  if (stamp %in% ls(envir = error_message_env)) {
    assign(stamp, NULL, envir = error_message_env)
  }
}

messageTracing <- function(x, stamp) {
  if (!stamp %in% ls(envir = error_message_env)) {
    assign(stamp, NULL, envir = error_message_env)
  }
  time_txt <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  history_message <- get(stamp, envir = error_message_env)
  x <- paste0("[", time_txt, "]", x)
  history_message <- c(history_message, x)
  assign(stamp, history_message, envir = error_message_env)
  cat(x)
}


running_time_stamp <- function(self_name = NULL) {
  stamp0 <- xfun::md5(Sys.time())
  if (!is.null(self_name) && is.character(self_name)) {
    stamp0 <- as.character(self_name)
  } else {
    stamp0 <- substr(stamp0, 1, 20)
  }
  paste0("backgroud_job_", stamp0)
}

check_memory_size_correct <- function(x) {
  .max_memory_useage <- x
  if (!is.null(.max_memory_useage)) {
    if (
      .max_memory_useage < 1000 * 1000 ||
        !is.numeric(.max_memory_useage)
    ) {
      stop(
        "You should provide `.max_memory_useage` use bytes and in integer.
        For Example, 1MB = 1,000,000bytes"
      )
    }
  }
}

is_valid_path <- function(path) {
  tryCatch(
    {
      normalizePath(path, mustWork = FALSE)
      TRUE
    },
    error = function(e) {
      FALSE
    }
  )
}

bgSave <- function(object, file, saveTo = "qs2"){
  if (saveTo == "qs2") {
    if (!"qs2" %in% rownames(installed.packages())) {
      stop("qs2 method should install qs2 package form CRAN. ")
    }
    saveFUN <- qs2::qs_save
    extName_of_savedFile <- ".qs2"
  } else {
    saveFUN <- base::saveRDS
    extName_of_savedFile <- ".rds"
  }
  obj_save_path00 <- paste0(file, extName_of_savedFile)
  saveFUN(object, obj_save_path00)
  invisible(obj_save_path00)
}

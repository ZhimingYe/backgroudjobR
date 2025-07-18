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
  stamp0 <- md5_one(Sys.time())
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

bgSave <- function(object, file, saveTo = "qs2") {
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


# Memory code cited from
# https://github.com/r-lib/lobstr/blob/main/R/mem.R

Mem_used <- function() {
  sum(gc()[, 1] * c(Node_size(), 8))
}

Node_size <- function() {
  bit <- 8L * .Machine$sizeof.pointer
  if (!(bit == 32L || bit == 64L)) {
    stop("Unknown architecture", call. = FALSE)
  }

  if (bit == 32L) 28L else 56L
}


# md5_one is cited from:
# https://github.com/cran/xfun/blob/eb9b401e26de0717944111bffc57fdc4fc7997c8/R/cache.R#L406
md5_one <- function(x) {
  # no need to write to a file if md5sum() has the 'bytes' arg (R > 4.4.1)
  m <- tools::md5sum
  if ('bytes' %in% names(formals(m))) {
    f <- NULL
  } else {
    f <- tempfile()
    on.exit(unlink(f), add = TRUE)
  }
  s <- serialize(x, NULL, xdr = FALSE)
  s <- tail(s, -14) # the first 14 bytes contain version info, etc
  if (is.null(f)) {
    m(bytes = s)
  } else {
    writeBin(s, f)
    unname(m(f))
  }
}

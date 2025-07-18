#' Execute R Code File with Error Logging and Auto-Save
#'
#' Executes R code from a file in an isolated environment with comprehensive
#' error handling, logging, and optional memory monitoring. This function is
#' ideal for running R scripts safely with automatic error recovery.
#'
#' @param .file Character. Path to the R script file to execute.
#' @param .external_val List. Named list of external variables to make available
#'   in the execution environment. Default is NULL.
#' @param .job_name Character. Optional name for the job used in logging and
#'   identification. If NULL, a timestamp-based name will be generated.
#' @param .save_when_error Logical. Whether to save the environment state when
#'   a fatal error occurs. Useful for debugging but requires sufficient disk space.
#'   Default is TRUE.
#' @param .save_method Character. Save method for error state. Either "qs2" for
#'   faster compression or "rds" for standard R serialization. Default is "qs2".
#' @param .check_memory_useage Logical. Whether to monitor and report memory
#'   usage in log files. Default is FALSE.
#' @param .max_memory_useage Numeric. Maximum memory usage in bytes. Job will
#'   be stopped if this limit is exceeded. Default is NULL (no limit).
#' @param .gc_span Numeric. Interval for garbage collection. If > 0, performs
#'   \code{gc()} every N lines of code. Default is 0 (no automatic GC).
#'
#' @return Character string containing the job history (logs)
#'
#' @details
#' The function creates an isolated execution environment and runs the specified
#' R script with comprehensive error handling. All execution details are logged,
#' and in case of errors, the environment can be automatically saved for debugging.
#'
#' @author Zhiming Ye
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic usage - run a simple script
#' job_history <- run_local_job(.file = "analysis.R")
#'
#' # Run script with external variables
#' job_history <- run_local_job(
#'   .file = "data_processing.R",
#'   .external_val = list(
#'     input_data = mtcars,
#'     threshold = 0.05,
#'     output_dir = "results/"
#'   ),
#'   .job_name = "mtcars_analysis"
#' )
#'
#' # Run with memory monitoring and limits
#' job_history <- run_local_job(
#'   .file = "memory_intensive.R",
#'   .check_memory_useage = TRUE,
#'   .max_memory_useage = 8e9,  # 8GB limit
#'   .gc_span = 10,  # GC every 10 lines
#'   .save_when_error = TRUE,
#'   .save_method = "qs2"
#' )
#'
#' # Check job history
#' print(paste("Job completed with ID:", job_history))
#' }
#'
run_local_job <- function(
  .file,
  .external_val = NULL,
  .job_name = NULL,
  .save_when_error = T,
  .save_method = c("qs2", "rds"),
  .check_memory_useage = F,
  .max_memory_useage = NULL,
  .gc_span = 0
) {
  check_memory_size_correct(.max_memory_useage)
  id <- do_job0(
    file = .file,
    ExternalVariables = .external_val,
    jobName = .job_name,
    saveTo = match.arg(.save_method),
    checkMem = .check_memory_useage,
    maxMemory = .max_memory_useage,
    performSaving = .save_when_error,
    fetchSessionInfo = T,
    activeGC = .gc_span
  )
  invisible(id)
}



#' Execute R Code File as Background Job
#'
#' Executes R code from a file in an isolated environment as a background process,
#' similar to RStudio's background job feature. Powered by the \code{mirai} package
#' for efficient parallel execution with comprehensive error handling and logging.
#'
#' @param .file Character. Path to the R script file to execute.
#' @param .target_dictionary Character. Working directory for the R script execution.
#'   If NULL, uses current working directory.
#' @param .external_val List. Named list of external variables to make available
#'   in the execution environment. Default is NULL.
#' @param .job_name Character. Optional name for the job used in logging and
#'   identification. If NULL, a timestamp-based name will be generated.
#' @param .save_when_error Logical. Whether to save the environment state when
#'   a fatal error occurs. Default is TRUE.
#' @param .save_method Character. Save method for error state. Either "qs2" for
#'   faster compression or "rds" for standard R serialization. Default is "qs2".
#' @param .check_memory_useage Logical. Whether to monitor and report memory
#'   usage in log files. Default is FALSE.
#' @param .max_memory_useage Numeric. Maximum memory usage in bytes. Job will
#'   be stopped if this limit is exceeded. Default is NULL (no limit).
#' @param .gc_span Numeric. Interval for garbage collection. If > 0, performs
#'   \code{gc()} every N lines of code. Default is 0 (no automatic GC).
#'
#' @return A \code{mirai} object representing the background job. Use
#'   \code{mirai::unresolved()} to check completion status and retrieve results.
#'   The object's data slot (\code{object$data}) contains logs.
#'
#' @details
#' This function requires active \code{mirai} daemons to be running. Use
#' \code{mirai::daemons(n)} to create background processes before calling this
#' function. Multiple jobs can be submitted and will be automatically distributed
#' across available daemons for parallel execution.
#'
#' The function changes the working directory to \code{.target_dictionary} during
#' execution and restores the original directory afterwards.
#'
#' @note
#' Ensure \code{mirai} daemons are active before using this function.
#' Use \code{mirai::daemons(0)} to properly close daemons when finished.
#'
#' @seealso
#' \url{https://mirai.r-lib.org/} for more details about the mirai package.
#'
#' @author Zhiming Ye
#' @export
#'
#' @examples
#' \dontrun{
#' # Setup mirai daemons for background execution
#' library(mirai)
#' mirai::daemons(3)  # Create 3 background processes
#'
#' # Submit a simple background job
#' job1 <- run_background_job(
#'   .file = "analysis.R",
#'   .target_dictionary = "/path/to/project",
#'   .job_name = "background_analysis"
#' )
#'
#' # Or submit multiple jobs with external variables
#' datasets <- list(mtcars, iris, airquality)
#' jobs <- list()
#' for (i in seq_along(datasets)) {
#'   jobs[[i]] <- run_background_job(
#'     .file = "process_data.R",
#'     .target_dictionary = paste0("project_", i),
#'     .external_val = list(data = datasets[[i]], id = i),
#'     .job_name = paste0("data_job_", i),
#'     .check_memory_useage = TRUE,
#'     .max_memory_useage = 4e9  # 4GB limit
#'   )
#' }
#'
#' # Check job status
#' print(mirai::unresolved(job1))  # Check if job1 is complete
#'
#' # Wait for all jobs to complete
#' results <- lapply(jobs, function(job) {
#'   while (!mirai::unresolved(job)) {
#'     Sys.sleep(1)  # Wait 1 second before checking again
#'   }
#'   return(job$data)  # Get running logs
#' })
#'
#' # Clean up daemons
#' mirai::daemons(0)
#' }
#'
run_background_job <- function(
  .file,
  .target_dictionary = NULL,
  .external_val = NULL,
  .job_name = NULL,
  .save_when_error = T,
  .save_method = c("qs2", "rds"),
  .check_memory_useage = F,
  .max_memory_useage = NULL,
  .gc_span = 0
) {
  now_dic <- getwd()
  if (is.null(.target_dictionary)) {
    .target_dictionary <- getwd()
  }
  if (!is_valid_path(.target_dictionary)) {
    stop("Not accessable dictionary provided. ")
  }
  check_memory_size_correct(.max_memory_useage)
  .save_method0 <- match.arg(.save_method)
  if (!mirai::require_daemons()) {
    stop("Please check your `mirai` daemons. ")
  }
  m <- mirai::mirai(
    {
      setwd(.target_dictionary)
      outmsg <- backgroudjobR::run_local_job(
        .file = .file,
        .external_val = .external_val,
        .job_name = .job_name,
        .save_when_error = .save_when_error,
        .save_method = .save_method0,
        .check_memory_useage = .check_memory_useage,
        .max_memory_useage = .max_memory_useage,
        .gc_span = .gc_span
      )
      setwd(now_dic)
      return(outmsg)
    },
    .file = .file,
    .external_val = .external_val,
    .job_name = .job_name,
    .save_method0 = .save_method0,
    .check_memory_useage = .check_memory_useage,
    .max_memory_useage = .max_memory_useage,
    now_dic = now_dic,
    .target_dictionary = .target_dictionary,
    .gc_span = .gc_span
  )
  invisible(m)
}


#' Create Bash-Executable R Job Project
#'
#' Creates a standalone R project that can be executed in bash/terminal using
#' tools like \code{screen} or \code{tmux} for persistent execution. This function
#' generates all necessary files for running R jobs in server environments or
#' for long-running processes that need to persist beyond the current R session.
#'
#' @param .file Character. Path to the R script file to execute.
#' @param .target_dictionary Character. Target directory where the project will
#'   be created. Must be provided and accessible. The directory will be created
#'   if it doesn't exist.
#' @param .external_val List. Named list of external variables to make available
#'   in the execution environment. These will be saved to a file and loaded
#'   during execution. Default is NULL.
#' @param .job_name Character. Optional name for the job used in logging and
#'   identification. If NULL, a timestamp-based name will be generated.
#' @param .save_when_error Logical. Whether to save the environment state when
#'   a fatal error occurs. Default is TRUE.
#' @param .save_method Character. Save method for error state and external variables.
#'   Either "qs2" for faster compression or "rds" for standard R serialization.
#'   Default is "qs2".
#' @param .check_memory_useage Logical. Whether to monitor and report memory
#'   usage in log files. Default is FALSE.
#' @param .max_memory_useage Numeric. Maximum memory usage in bytes. Job will
#'   be stopped if this limit is exceeded. Default is NULL (no limit).
#' @param .gc_span Numeric. Interval for garbage collection. If > 0, performs
#'   \code{gc()} every N lines of code. Default is 0 (no automatic GC).
#'
#' @return Invisible NULL. The function creates files in the target directory
#'   and prints instructions for execution.
#'
#' @details
#' This function creates a complete R project structure in the specified directory:
#' \itemize{
#'   \item \code{run.R}: Main execution script
#'   \item \code{bg_job_external.files.*}: Serialized external variables (if provided)
#' }
#'
#' The generated project can be executed independently using:
#' \code{Rscript run.R}
#'
#' For persistent execution on servers, use:
#' \itemize{
#'   \item \code{screen -S job_name Rscript run.R}
#'   \item \code{tmux new-session -d -s job_name 'Rscript run.R'}
#'   \item \code{nohup Rscript run.R > output.log 2>&1 &}
#' }
#'
#' @note
#' If \code{run.R} already exists in the target directory, a new file with
#' a random suffix will be created to avoid conflicts.
#'
#' @author Zhiming Ye
#' @export
#'
#' @examples
#' \dontrun{
#' # Create a simple bash job project
#' run_bash_job(
#'   .file = "long_running_analysis.R",
#'   .target_dictionary = "~/projects/analysis_job",
#'   .job_name = "genomics_analysis"
#' )
#'
#' # Create project with external data and memory monitoring
#' large_dataset <- read.csv("big_data.csv")
#' config <- list(threads = 8, output_format = "csv")
#'
#' run_bash_job(
#'   .file = "data_processing.R",
#'   .target_dictionary = "/scratch/user/processing_job",
#'   .external_val = list(
#'     data = large_dataset,
#'     config = config,
#'     timestamp = Sys.time()
#'   ),
#'   .job_name = "big_data_processing",
#'   .check_memory_useage = TRUE,
#'   .max_memory_useage = 32e9,  # 32GB limit
#'   .save_method = "qs2"
#' )
#'
#' # Execute the created project (run these commands in terminal):
#' # cd ~/projects/analysis_job
#' # screen -S analysis Rscript run.R
#' # # Detach with Ctrl+A, D
#' # # Reattach with: screen -r analysis
#'
#' # Alternative execution methods:
#' # tmux new-session -d -s analysis 'Rscript run.R'
#' # nohup Rscript run.R > analysis.log 2>&1 &
#' }
#'
run_bash_job <- function(
  .file,
  .target_dictionary = NULL,
  .external_val = NULL,
  .job_name = NULL,
  .save_when_error = T,
  .save_method = c("qs2", "rds"),
  .check_memory_useage = F,
  .max_memory_useage = NULL,
  .gc_span = 0
) {
  .save_method <- match.arg(.save_method)
  if (is.null(.target_dictionary) || (!is_valid_path(.target_dictionary))) {
    stop("Please provide `.target_dictionary` command and ensure it is empty. ")
  }
  createPath(.target_dictionary)
  if (!is.null(.external_val) && identical(is.list(.external_val), F)) {
    stop("`.external_val` should be a single list. ")
  }
  if (!is.null(.external_val) && identical(is.list(.external_val), T)) {
    bgSave(
      .external_val,
      paste0(.target_dictionary, "/bg_job_external.files"),
      saveTo = .save_method
    )
    if (.save_method == "qs2") {
      read_method <- glue::glue(
        "qs2::qs_read('{.target_dictionary}/bg_job_external.files.qs2')"
      )
    } else {
      read_method <- glue::glue(
        "readRDS('{.target_dictionary}/bg_job_external.files.rds')"
      )
    }
  } else {
    read_method <- "NULL"
  }

  outputDoc <- glue::glue(
    "
  backgroudjobR::run_local_job(
        .file = {deparse(.file)},
        .external_val = {read_method},
        .job_name = {deparse(.job_name)},
        .save_when_error = {deparse(.save_when_error)},
        .save_method = {deparse(.save_method)},
        .check_memory_useage = {deparse(.check_memory_useage)},
        .max_memory_useage = {deparse(.max_memory_useage)},
        .gc_span = {deparse(.gc_span)}
      )
  "
  )
  if (file.exists(paste0(.target_dictionary, "/run.R"))) {
    new_file_name <- substr(md5_one(Sys.time()), 1, 5)
    fn2 <- paste0(.target_dictionary, "/", new_file_name, "_run.R")
    writeLines(outputDoc, fn2)
    warning(glue::glue("run.R existed. Automatically rename to {fn2}"))
  }
  writeLines(outputDoc, paste0(.target_dictionary, "/run.R"))
  message(glue::glue(
    "Successfully created project in dictionary {.target_dictionary}.
          Please use `screen` or `tmux` in terminal
                     to execute `Rscript run.R` for persistent running. "
  ))
}

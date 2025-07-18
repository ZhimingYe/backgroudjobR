% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/interface.R
\name{run_background_job}
\alias{run_background_job}
\title{Execute R Code File as Background Job}
\usage{
run_background_job(
  .file,
  .target_dictionary = NULL,
  .external_val = NULL,
  .job_name = NULL,
  .save_when_error = T,
  .save_method = c("qs2", "rds"),
  .check_memory_useage = F,
  .max_memory_useage = NULL,
  .gc_span = 0
)
}
\arguments{
\item{.file}{Character. Path to the R script file to execute.}

\item{.target_dictionary}{Character. Working directory for the R script execution.
If NULL, uses current working directory.}

\item{.external_val}{List. Named list of external variables to make available
in the execution environment. Default is NULL.}

\item{.job_name}{Character. Optional name for the job used in logging and
identification. If NULL, a timestamp-based name will be generated.}

\item{.save_when_error}{Logical. Whether to save the environment state when
a fatal error occurs. Default is TRUE.}

\item{.save_method}{Character. Save method for error state. Either "qs2" for
faster compression or "rds" for standard R serialization. Default is "qs2".}

\item{.check_memory_useage}{Logical. Whether to monitor and report memory
usage in log files. Default is FALSE.}

\item{.max_memory_useage}{Numeric. Maximum memory usage in bytes. Job will
be stopped if this limit is exceeded. Default is NULL (no limit).}

\item{.gc_span}{Numeric. Interval for garbage collection. If > 0, performs
\code{gc()} every N lines of code. Default is 0 (no automatic GC).}
}
\value{
A \code{mirai} object representing the background job. Use
  \code{mirai::unresolved()} to check completion status and retrieve results.
  The object's data slot (\code{object$data}) contains logs.
}
\description{
Executes R code from a file in an isolated environment as a background process,
similar to RStudio's background job feature. Powered by the \code{mirai} package
for efficient parallel execution with comprehensive error handling and logging.
}
\details{
This function requires active \code{mirai} daemons to be running. Use
\code{mirai::daemons(n)} to create background processes before calling this
function. Multiple jobs can be submitted and will be automatically distributed
across available daemons for parallel execution.

The function changes the working directory to \code{.target_dictionary} during
execution and restores the original directory afterwards.
}
\note{
Ensure \code{mirai} daemons are active before using this function.
Use \code{mirai::daemons(0)} to properly close daemons when finished.
}
\examples{
\dontrun{
# Setup mirai daemons for background execution
library(mirai)
mirai::daemons(3)  # Create 3 background processes

# Submit a simple background job
job1 <- run_background_job(
  .file = "analysis.R",
  .target_dictionary = "/path/to/project",
  .job_name = "background_analysis"
)

# Or submit multiple jobs with external variables
datasets <- list(mtcars, iris, airquality)
jobs <- list()
for (i in seq_along(datasets)) {
  jobs[[i]] <- run_background_job(
    .file = "process_data.R",
    .target_dictionary = paste0("project_", i),
    .external_val = list(data = datasets[[i]], id = i),
    .job_name = paste0("data_job_", i),
    .check_memory_useage = TRUE,
    .max_memory_useage = 4e9  # 4GB limit
  )
}

# Check job status
print(mirai::unresolved(job1))  # Check if job1 is complete

# Wait for all jobs to complete
results <- lapply(jobs, function(job) {
  while (!mirai::unresolved(job)) {
    Sys.sleep(1)  # Wait 1 second before checking again
  }
  return(job$data)  # Get running logs
})

# Clean up daemons
mirai::daemons(0)
}

}
\seealso{
\url{https://mirai.r-lib.org/} for more details about the mirai package.
}
\author{
Zhiming Ye
}

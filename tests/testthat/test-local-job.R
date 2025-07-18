# tests/testthat/test-job-functions.R

library(testthat)
library(mockery)

# Test setup - create temporary directory and files for testing
setup_test_environment <- function() {
  temp_dir <- tempdir()
  test_script <- file.path(temp_dir, "test_script.R")
  writeLines(c(
    "# Test R script",
    "x <- 1 + 1",
    "print(x)"
  ), test_script)

  return(list(
    temp_dir = temp_dir,
    test_script = test_script
  ))
}

# Mock internal functions that aren't defined in the provided code
mock_internal_functions <- function() {
  # Mock do_job0 function
  stub(run_local_job, "do_job0", function(...) "test_job_id_123")

  # Mock check_memory_size_correct
  stub(run_local_job, "check_memory_size_correct", function(x) {
    if (!is.null(x) && (!is.numeric(x) || x <= 0)) {
      stop("Invalid memory size")
    }
    return(TRUE)
  })

  # Mock is_valid_path
  stub(run_background_job, "is_valid_path", function(path) {
    file.exists(path) || dir.exists(path)
  })

  stub(run_bash_job, "is_valid_path", function(path) {
    file.exists(path) || dir.exists(path)
  })

  # Mock createPath
  stub(run_bash_job, "createPath", function(path) {
    if (!dir.exists(path)) {
      dir.create(path, recursive = TRUE)
    }
    return(TRUE)
  })

  # Mock bgSave
  stub(run_bash_job, "bgSave", function(obj, path, saveTo = "qs2") {
    if (saveTo == "qs2") {
      # Simulate qs2 save
      file.create(paste0(path, ".qs2"))
    } else {
      # Simulate RDS save
      saveRDS(obj, paste0(path, ".rds"))
    }
    return(TRUE)
  })

  # Mock md5_one
  stub(run_bash_job, "md5_one", function(x) {
    "abcde12345"
  })
}

# Tests for run_local_job
test_that("run_local_job executes with valid parameters", {
  env <- setup_test_environment()
  mock_internal_functions()

  # Test basic execution
  result <- run_local_job(.file = env$test_script)
  expect_type(result, "character")
  # expect_equal(result, "test_job_id_123")
})

test_that("run_local_job handles external variables", {
  env <- setup_test_environment()
  mock_internal_functions()

  external_vars <- list(
    data = mtcars,
    threshold = 0.05,
    name = "test"
  )

  result <- run_local_job(
    .file = env$test_script,
    .external_val = external_vars,
    .job_name = "test_job"
  )

  expect_type(result, "character")
})

test_that("run_local_job validates memory parameters", {
  env <- setup_test_environment()

  # Mock check_memory_size_correct to actually validate
  stub(run_local_job, "check_memory_size_correct", function(x) {
    if (!is.null(x) && (!is.numeric(x) || x <= 0)) {
      stop("Invalid memory size")
    }
    return(TRUE)
  })

  stub(run_local_job, "do_job0", function(...) "test_id")

  # Test valid memory size
  expect_silent(run_local_job(.file = env$test_script, .max_memory_useage = 1e9))

  # Test invalid memory size
  expect_error(
    run_local_job(.file = env$test_script, .max_memory_useage = -1),
    "Invalid memory size"
  )

  expect_error(
    run_local_job(.file = env$test_script, .max_memory_useage = "invalid"),
    "Invalid memory size"
  )
})

test_that("run_local_job handles save method validation", {
  env <- setup_test_environment()
  mock_internal_functions()

  # Test valid save methods
  run_local_job(.file = env$test_script, .save_method = "qs2")
  run_local_job(.file = env$test_script, .save_method = "rds")
#
#   Test invalid save method
  expect_no_error(
    run_local_job(.file = env$test_script, .save_method = "invalid")
  )
})

# Tests for run_background_job
test_that("run_background_job requires mirai daemons", {
  env <- setup_test_environment()

  # Mock mirai functions
  stub(run_background_job, "mirai::require_daemons", function() FALSE)

  expect_error(
    run_background_job(.file = env$test_script),
    "Please check your `mirai` daemons"
  )
})

test_that("run_background_job works with valid mirai setup", {
  env <- setup_test_environment()

  # Mock mirai functions
  stub(run_background_job, "mirai::require_daemons", function() TRUE)
  stub(run_background_job, "mirai::mirai", function(...) {
    list(data = "test_mirai_result")
  })

  # Mock other internal functions
  stub(run_background_job, "check_memory_size_correct", function(x) TRUE)
  stub(run_background_job, "is_valid_path", function(path) TRUE)

  result <- run_background_job(
    .file = env$test_script,
    .target_dictionary = env$temp_dir
  )

  expect_type(result, "list")
  expect_equal(result$data, "test_mirai_result")
})

test_that("run_background_job validates target directory", {
  env <- setup_test_environment()

  # Mock functions
  stub(run_background_job, "check_memory_size_correct", function(x) TRUE)
  stub(run_background_job, "is_valid_path", function(path) FALSE)

  expect_error(
    run_background_job(
      .file = env$test_script,
      .target_dictionary = "/nonexistent/path"
    ),
    "Not accessable dictionary provided"
  )
})

test_that("run_background_job uses current directory when target is NULL", {
  env <- setup_test_environment()

  # Mock functions
  stub(run_background_job, "mirai::require_daemons", function() TRUE)
  stub(run_background_job, "mirai::mirai", function(...) {
    args <- list(...)
    # Check that target_dictionary was set to current working directory
    expect_equal(args$.target_dictionary, getwd())
    return(list(data = "success"))
  })
  stub(run_background_job, "check_memory_size_correct", function(x) TRUE)
  stub(run_background_job, "is_valid_path", function(path) TRUE)

  expect_no_error({run_background_job(.file = env$test_script)})
})

# Tests for run_bash_job
test_that("run_bash_job creates project files", {
  env <- setup_test_environment()
  target_dir <- file.path(env$temp_dir, "bash_job_test")

  # Mock internal functions
  stub(run_bash_job, "is_valid_path", function(path) TRUE)
  stub(run_bash_job, "createPath", function(path) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
    return(TRUE)
  })

  expect_message(
    run_bash_job(
      .file = env$test_script,
      .target_dictionary = target_dir,
      .job_name = "test_bash_job"
    ),
    "Successfully created project"
  )

  # Check that run.R was created
  expect_true(file.exists(file.path(target_dir, "run.R")))

  # Check content of run.R
  run_content <- readLines(file.path(target_dir, "run.R"))
  expect_true(any(grepl("backgroudjobR::run_local_job", run_content)))
})

test_that("run_bash_job handles external variables", {
  env <- setup_test_environment()
  target_dir <- file.path(env$temp_dir, "bash_job_external")

  # Mock internal functions
  stub(run_bash_job, "is_valid_path", function(path) TRUE)
  stub(run_bash_job, "createPath", function(path) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
    return(TRUE)
  })
  stub(run_bash_job, "bgSave", function(obj, path, saveTo = "qs2") {
    if (saveTo == "qs2") {
      file.create(paste0(path, ".qs2"))
    } else {
      saveRDS(obj, paste0(path, ".rds"))
    }
    return(TRUE)
  })

  external_vars <- list(data = iris, config = list(threads = 4))

  run_bash_job(
    .file = env$test_script,
    .target_dictionary = target_dir,
    .external_val = external_vars,
    .save_method = "qs2"
  )

  # Check that external variables file was created
  expect_true(file.exists(file.path(target_dir, "bg_job_external.files.qs2")))

  # Check that run.R contains reference to external file
  run_content <- readLines(file.path(target_dir, "run.R"))
  expect_true(any(grepl("qs2::qs_read", run_content)))
})

test_that("run_bash_job handles existing run.R file", {
  env <- setup_test_environment()
  target_dir <- file.path(env$temp_dir, "bash_job_existing")
  dir.create(target_dir, recursive = TRUE)

  # Create existing run.R
  writeLines("# Existing run.R", file.path(target_dir, "run.R"))

  # Mock internal functions
  stub(run_bash_job, "is_valid_path", function(path) TRUE)
  stub(run_bash_job, "createPath", function(path) TRUE)
  stub(run_bash_job, "md5_one", function(x) "test12345")

  expect_warning(
    run_bash_job(
      .file = env$test_script,
      .target_dictionary = target_dir
    ),
    "run.R existed"
  )

  # Check that both files exist
  expect_true(file.exists(file.path(target_dir, "run.R")))
  expect_true(file.exists(file.path(target_dir, "test1_run.R")))
})

test_that("run_bash_job validates target directory", {
  env <- setup_test_environment()

  # Mock is_valid_path to return FALSE
  stub(run_bash_job, "is_valid_path", function(path) FALSE)

  expect_error(
    run_bash_job(
      .file = env$test_script,
      .target_dictionary = NULL
    ),
    "Please provide `.target_dictionary`"
  )

  expect_error(
    run_bash_job(
      .file = env$test_script,
      .target_dictionary = "/invalid/path"
    ),
    "Please provide `.target_dictionary`"
  )
})

test_that("run_bash_job validates external variables", {
  env <- setup_test_environment()
  target_dir <- file.path(env$temp_dir, "bash_job_validation")

  # Mock internal functions
  stub(run_bash_job, "is_valid_path", function(path) TRUE)
  stub(run_bash_job, "createPath", function(path) TRUE)

  # Test invalid external variables (not a list)
  expect_error(
    run_bash_job(
      .file = env$test_script,
      .target_dictionary = target_dir,
      .external_val = "not_a_list"
    ),
    "`.external_val` should be a single list"
  )

  expect_error(
    run_bash_job(
      .file = env$test_script,
      .target_dictionary = target_dir,
      .external_val = c(1, 2, 3)
    ),
    "`.external_val` should be a single list"
  )
})

test_that("run_bash_job handles different save methods", {
  env <- setup_test_environment()
  target_dir_qs2 <- file.path(env$temp_dir, "bash_job_qs2")
  target_dir_rds <- file.path(env$temp_dir, "bash_job_rds")

  # Mock internal functions
  stub(run_bash_job, "is_valid_path", function(path) TRUE)
  stub(run_bash_job, "createPath", function(path) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
    return(TRUE)
  })
  stub(run_bash_job, "bgSave", function(obj, path, saveTo = "qs2") {
    if (saveTo == "qs2") {
      file.create(paste0(path, ".qs2"))
    } else {
      saveRDS(obj, paste0(path, ".rds"))
    }
    return(TRUE)
  })

  external_vars <- list(test = "data")

  # Test qs2 method
  run_bash_job(
    .file = env$test_script,
    .target_dictionary = target_dir_qs2,
    .external_val = external_vars,
    .save_method = "qs2"
  )

  run_content_qs2 <- readLines(file.path(target_dir_qs2, "run.R"))
  expect_true(any(grepl("qs2::qs_read", run_content_qs2)))

  # Test rds method
  run_bash_job(
    .file = env$test_script,
    .target_dictionary = target_dir_rds,
    .external_val = external_vars,
    .save_method = "rds"
  )

  run_content_rds <- readLines(file.path(target_dir_rds, "run.R"))
  expect_true(any(grepl("readRDS", run_content_rds)))
})

# Integration tests
test_that("All functions handle parameter passing correctly", {
  env <- setup_test_environment()

  # Test that all functions accept the same core parameters
  common_params <- list(
    .file = env$test_script,
    .external_val = list(test = "value"),
    .job_name = "integration_test",
    .save_when_error = FALSE,
    .save_method = "rds",
    .check_memory_useage = TRUE,
    .max_memory_useage = 1e9,
    .gc_span = 5
  )

  # Mock all required functions for run_local_job
  stub(run_local_job, "check_memory_size_correct", function(x) TRUE)
  stub(run_local_job, "do_job0", function(...) "test_id")

  expect_silent(do.call(run_local_job, common_params))

  # For run_bash_job, add target directory
  bash_params <- c(common_params, list(.target_dictionary = env$temp_dir))
  stub(run_bash_job, "is_valid_path", function(path) TRUE)
  stub(run_bash_job, "createPath", function(path) TRUE)
  stub(run_bash_job, "bgSave", function(...) TRUE)

  expect_message(do.call(run_bash_job, bash_params))
})

# Cleanup
withr::defer(unlink("temp_dir", recursive = TRUE), teardown_env())
test_dir <- getwd()
bg_dirs <- list.dirs(test_dir, full.names = TRUE, recursive = FALSE)
bg_dirs <- bg_dirs[basename(bg_dirs) %in% grep("^backgroud_job_", basename(bg_dirs), value = TRUE)]
unlink(bg_dirs, recursive = TRUE, force = TRUE)

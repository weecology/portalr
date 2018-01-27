context("Check data retrieval")

test_that("download_observations works", {
  expect_error(download_observations(".", release_only = FALSE), NA)
  expect_error(download_observations("."), NA)
})

test_that("load_data works", {
  expect_error(data_tables <- load_data("repo"), NA)
  expect_equal(length(data_tables), 5)
  expect_error(data_tables <- load_data("."), NA)
  expect_equal(length(data_tables), 5)
  curr_files <- list.files()
  chars <- mapply(substr, curr_files, seq(curr_files), seq(curr_files))
  diff_chars <- sapply(chars, function(x) if(x == "a") "b" else "a")
  bad_path <- file.path(getwd(), paste0(diff_chars, collapse = ""))
  expect_error(data_tables <- load_data(bad_path, download_if_missing = FALSE))
  dir.create(bad_path)
  expect_warning(data_tables <- load_data(bad_path))
  expect_equal(length(data_tables), 5)
})


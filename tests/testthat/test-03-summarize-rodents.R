# Check rodent data summaries

portal_data_path <- tempdir()

test_that("summarize_rodent_data returns expected results, and filters by plots correctly", {
  skip_on_cran()
  ab_all_plots <- summarize_rodent_data(path = portal_data_path, level = "plot",
                                        na_drop = TRUE)

  rodent_counts <- ab_all_plots %>%
    dplyr::filter(plot == 4) %>% dplyr::select(-"treatment", -"plot")

  ab_plot_4 <- summarize_rodent_data(path = portal_data_path, plots = 4,
                                     na_drop = TRUE, zero_drop = FALSE)

  expect_equal(rodent_counts, ab_plot_4)

  rodent_counts <- ab_all_plots %>%
    dplyr::filter(plot %in% c(4, 8, 10, 12)) %>%
    dplyr::select(-"treatment", -"plot") %>%
    tidyr::gather(species, abundance, BA:SO) %>%
    dplyr::count(period, species, wt = abundance) %>%
    tidyr::spread(species, n)

  ab_plots_4_8_10_12 <- summarize_rodent_data(path = portal_data_path, plots = c(4, 8, 10, 12),
                               na_drop = TRUE, zero_drop = FALSE)

  expect_equal(rodent_counts, ab_plots_4_8_10_12)
})

test_that("summarize_rodent_data gives warning for using length", {
  skip_on_cran()
  expect_warning(dat <- summarize_rodent_data(path = portal_data_path, length = "all"))
  expect_equal(dat, summarize_rodent_data(path = portal_data_path, plots = "all"))
})

test_that("abundance returns expected results", {
  skip_on_cran()
  ab_notfilled <- abundance(path = portal_data_path, level = "Plot", type = "Rodents",
                            plots = "all", unknowns = FALSE,
                            shape = "flat", time = "period", fillweight = FALSE,
                            na_drop = FALSE, zero_drop = TRUE, min_traps = 1,
                            min_plots = 24, effort = FALSE)
  test_ab <- dplyr::filter(ab_notfilled, period %in% 400:450)
  expect_equal(nrow(test_ab), 25704)
  expect_true(sum(test_ab$abundance, na.rm = TRUE) == 10110)
  test_ab <- dplyr::filter(ab_notfilled, species == "DM", abundance > 0)
  expect_equal(max(test_ab$abundance, na.rm = TRUE), 17)
  expect_false(anyNA(test_ab))

  ab_filled <- abundance(path = portal_data_path, level = "Plot", type = "Rodents",
                         plots = "all", unknowns = FALSE,
                         shape = "flat", time = "period", fillweight = TRUE,
                         na_drop = FALSE, zero_drop = TRUE, min_traps = 1,
                         min_plots = 24, effort = FALSE)
  expect_equal(ab_notfilled, ab_filled)
})

test_that("biomass returns expected results", {
  skip_on_cran()
  biom_filled <- biomass(path = portal_data_path, level = "Plot", type = "Rodents",
                         plots = "all", unknowns = FALSE,
                         shape = "flat", time = "period", fillweight = TRUE,
                         na_drop = FALSE, zero_drop = FALSE, min_traps = 1,
                         min_plots = 24, effort = FALSE) %>%
    dplyr::filter(period %in% 400:450)

  biom_notfilled <- biomass(path = portal_data_path, level = "Plot", type = "Rodents",
                            plots = "all", unknowns = FALSE,
                            shape = "flat", time = "period", fillweight = FALSE,
                            na_drop = FALSE, zero_drop = FALSE, min_traps = 1,
                            min_plots = 24, effort = FALSE) %>%
    dplyr::filter(period %in% 400:450)

  expect_equal(nrow(biom_filled), 25704)
  expect_equal(dim(biom_filled), dim(biom_notfilled))
  expect_equal(biom_filled$species, biom_notfilled$species)
  expect_equal(floor(dplyr::filter(biom_notfilled, period == 447, plot == 3,
                                   species == "BA")$biomass), 15)
  expect_equal(floor(dplyr::filter(biom_filled, period == 447, plot == 3,
                                   species == "BA")$biomass), 24)
})

test_that("energy returns expected results", {
  skip_on_cran()
  energy_filled <- energy(path = portal_data_path, level = "Plot", type = "Rodents",
                          plots = "all", unknowns = FALSE,
                          shape = "flat", time = "period", fillweight = TRUE,
                          na_drop = FALSE, zero_drop = FALSE, min_traps = 1,
                          min_plots = 24, effort = FALSE) %>%
    dplyr::filter(period %in% 400:450)

  energy_notfilled <- energy(path = portal_data_path, level = "Plot", type = "Rodents",
                             plots = "all", unknowns = FALSE,
                             shape = "flat", time = "period", fillweight = FALSE,
                             na_drop = FALSE, zero_drop = FALSE, min_traps = 1,
                             min_plots = 24, effort = FALSE) %>%
    dplyr::filter(period %in% 400:450)

  expect_equal(nrow(energy_filled), 25704)
  expect_equal(dim(energy_filled), dim(energy_notfilled))
  expect_equal(energy_filled$species, energy_notfilled$species)
  expect_equal(floor(dplyr::filter(energy_notfilled, period == 447, plot == 3,
                                   species == "BA")$energy), 51)
  expect_equal(floor(dplyr::filter(energy_filled, period == 447, plot == 3,
                                   species == "BA")$energy), 82)
})

test_that("rates returns expected results", {
  skip_on_cran()
  rates_default <- rates(path = portal_data_path) %>%
    dplyr::filter(period %in% 400:450)

  rates_flat <- rates(path = portal_data_path, level = "Plot", type = "Rodents",
                             plots = "all", unknowns = FALSE,
                             shape = "flat", time = "period", fillweight = FALSE,
                             na_drop = FALSE, zero_drop = FALSE, min_traps = 1,
                             min_plots = 24, effort = FALSE) %>%
    dplyr::filter(period %in% 400:450)

  expect_equal(nrow(rates_default), 42)
  expect_equal(nrow(rates_flat), 25704)
  expect_equal(floor(dplyr::filter(rates_default, period == 440)$DM), -1)
  expect_equal(floor(dplyr::filter(rates_flat, period == 446, plot == 11,
                                   species == "DM")$rates), 0)
})

test_that("abundance filters at the plot level correctly", {
  skip_on_cran()
  incomplete_plots <- abundance(path = portal_data_path, level = "plot",
                                min_plots = 1, min_traps = 1, effort = TRUE,
                                na_drop = FALSE) %>%
    dplyr::filter(ntraps < 1, period <= 463)
  expect_equal(NROW(incomplete_plots), 238)

  incomplete_plots <- abundance(path = portal_data_path, level = "plot",
                                min_plots = 1, min_traps = 1, effort = TRUE) %>%
    dplyr::filter(ntraps < 1, period <= 463)
  expect_equal(NROW(incomplete_plots), 0)

  incomplete_plots <- abundance(path = portal_data_path, level = "plot",
                                min_plots = 24, min_traps = 49, effort = TRUE) %>%
    dplyr::filter(ntraps < 1, period <= 463)
  expect_equal(NROW(incomplete_plots), 0)

  incomplete_plots <- abundance(path = portal_data_path, level = "plot", na_drop = TRUE,
                                min_plots = 1, min_traps = 1, effort = TRUE) %>%
    dplyr::filter(ntraps < 1, period <= 463)
  expect_equal(NROW(incomplete_plots), 0)

  incomplete_plots <- abundance(path = portal_data_path, level = "plot", na_drop = TRUE,
                                min_plots = 1, min_traps = 47, effort = TRUE) %>%
    dplyr::filter(ntraps < 47, period <= 463)
  expect_equal(NROW(incomplete_plots), 0)

  incomplete_plots <- abundance(path = portal_data_path, level = "plot", na_drop = TRUE,
                                min_plots = 1, min_traps = 49, effort = TRUE) %>%
    dplyr::filter(ntraps < 49, period <= 463)
  expect_equal(NROW(incomplete_plots), 0)

  incomplete_plots <- abundance(path = portal_data_path, level = "plot", na_drop = TRUE,
                                min_plots = 12, min_traps = 1, effort = TRUE) %>%
    dplyr::filter(ntraps < 1, period <= 463)
  expect_equal(NROW(incomplete_plots), 0)

  incomplete_plots <- abundance(path = portal_data_path, level = "plot", na_drop = TRUE,
                                min_plots = 12, min_traps = 47, effort = TRUE) %>%
    dplyr::filter(ntraps < 47, period <= 463)
  expect_equal(NROW(incomplete_plots), 0)

  incomplete_plots <- abundance(path = portal_data_path, level = "plot", na_drop = TRUE,
                                min_plots = 12, min_traps = 49, effort = TRUE) %>%
    dplyr::filter(ntraps < 49, period <= 463)
  expect_equal(NROW(incomplete_plots), 0)

  incomplete_plots <- abundance(path = portal_data_path, level = "plot", na_drop = TRUE,
                                min_plots = 24, min_traps = 1, effort = TRUE) %>%
    dplyr::filter(ntraps < 1, period <= 463)
  expect_equal(NROW(incomplete_plots), 0)

  incomplete_plots <- abundance(path = portal_data_path, level = "plot", na_drop = TRUE,
                                min_plots = 24, min_traps = 47, effort = TRUE) %>%
    dplyr::filter(ntraps < 47, period <= 463)
  expect_equal(NROW(incomplete_plots), 0)

  incomplete_plots <- abundance(path = portal_data_path, level = "plot", na_drop = TRUE,
                                min_plots = 24, min_traps = 49, effort = TRUE) %>%
    dplyr::filter(ntraps < 49, period <= 463)
  expect_equal(NROW(incomplete_plots), 0)
})



test_that("rodent_species provides proper vectors or data frame", {
  skip_on_cran()

  rodents <- rodent_species()
  expect_length(rodents, 30)

  all_rodents <- rodent_species(set = "all")
  expect_length(all_rodents, 30)

  fc_rodents <- rodent_species(set = "forecasting")
  expect_length(fc_rodents, 20)

  fct_rodents <- rodent_species(set = "forecasting", total = TRUE)
  expect_length(fct_rodents, 21)

  expect_type(rodents, "character")
  expect_type(all_rodents, "character")
  expect_type(fc_rodents, "character")
  expect_type(fct_rodents, "character")

  rodent_abbr <- rodent_species(type = "abbreviation")
  expect_type(rodent_abbr, "character")
  expect_equal(unique(nchar(rodent_abbr)), 2)

  rodent_comm <- rodent_species(type = "common")
  expect_type(rodent_comm , "character")
  expect_equal(unique(nchar(rodent_comm)), c(26, 20, 21, 19, 17, 31, 22, 18, 12, 10, 13, 24, 23, 28))

  rodent_scie <- rodent_species(type = "scientific")
  expect_type(rodent_scie, "character")
  expect_equal(unique(nchar(rodent_scie)), c(24, 15, 19, 20, 23, 26, 18, 13, 21, 16, 22, 25, 10, 17, 12, 29))

  rodent_gs <- rodent_species(type = "g_species")
  expect_type(rodent_gs, "character")
  expect_equal(unique(nchar(rodent_gs)), c(10, 11, 14, 15, 6, 8, 9, 13, 12))

  rodent_t <- rodent_species(type = "table")
  expect_s3_class(rodent_t, "data.frame")
  expect_equal(dim(rodent_t), c(30, 4))

  rodent_tt <- rodent_species(type = "table", total = TRUE)
  expect_s3_class(rodent_tt, "data.frame")
  expect_equal(dim(rodent_tt), c(31, 4))

  expect_error(rodent_species(type = "error"))
  expect_error(rodent_species(set = "error"))


})



test_that("na_conformer makes NA into `NA` in vectors and data frames", {

  # work on vectors

    xx <- c("a", "b", NA, "c")
    expect_equal(na_conformer(xx)[3], "NA")

  # works on dfs

    xx <- data.frame(w = "a", n = as.character(c("d", NA, "a", "b", "c")))
    expect_s3_class(na_conformer(xx, "n"), "data.frame")
    expect_equal(na_conformer(xx, "n")[2,2], "NA")
})


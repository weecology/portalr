context("Check ant data summaries")

portal_data_path <- tempdir()

test_that("colony_presence_absence returns expected results", {
  colonypresabs.stake <- colony_presence_absence(path = portal_data_path, level = "Stake",
                                                rare_sp = T, unknowns = T)

  test.colony <- dplyr::filter(colonypresabs.stake, year %in% 2000:2005)
  expect_equal(sum(test.colony$presence, na.rm = T), 4930)
  expect_false(anyNA(dplyr::filter(test.colony, species != "camp fest")))
  expect_equal(nrow(test.colony), 190512)

  colonypresabs.site <- colony_presence_absence(path = portal_data_path, level = "Site", rare_sp = T, unknowns = T)
  expect_equal(unique(colonypresabs.site$species), unique(colonypresabs.stake$species))

})

test_that("bait_presence_absence returns expected results", {
  baitpresabs.stake <- bait_presence_absence(path = portal_data_path, level = "Stake")

  test.bait <- dplyr::filter(baitpresabs.stake, year %in% 2000:2005)
  expect_equal(sum(test.bait$presence, na.rm = T), 3420)
  expect_false(anyNA(test.bait))
  expect_equal(nrow(test.bait), 68400)

  baitpresabs.site <- bait_presence_absence(path = portal_data_path, level = "Site")
  expect_equal(sort(unique(as.character(baitpresabs.site$species))),
               sort(unique(as.character(baitpresabs.stake$species))))
})

test_that("colony_presence_absence returns expected results", {
  colonypresabs.plot <- colony_presence_absence(path = portal_data_path, level = "plot",
                                                 rare_sp = T, unknowns = T)

  test.colony <- dplyr::filter(colonypresabs.plot, year %in% 2000:2005)
  expect_equal(sum(test.colony$presence, na.rm = T), 1170)
  expect_false(anyNA(dplyr::filter(test.colony, species != "camp fest")))
  expect_equal(nrow(test.colony), 3888)

  colonypresabs.site <- colony_presence_absence(path = portal_data_path, level = "site",
                                                rare_sp = T, unknowns = T)
  expect_equal(sort(unique(as.character(colonypresabs.site$species))),
               sort(unique(as.character(colonypresabs.plot$species))))
})

test_that("bait_presence_absence returns expected results", {
  baitpresabs.plot <- bait_presence_absence(path = portal_data_path, level = "plot")

  test.bait <- dplyr::filter(baitpresabs.plot, year %in% 2000:2005)
  expect_equal(sum(test.bait$presence, na.rm = T), 739)
  expect_false(anyNA(test.bait))
  expect_equal(nrow(test.bait), 2736)

  baitpresabs.site <- bait_presence_absence(path = portal_data_path, level = "site")
  expect_equal(sort(unique(as.character(baitpresabs.site$species))),
               sort(unique(as.character(baitpresabs.plot$species))))
})

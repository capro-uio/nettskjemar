context("test-codebook")
library(dplyr)
library(tidyr)
library(tibble)

test_that("Does reading in codebook work", {
  codebook <- codebook_read("../codebook-82087-2018-09-03.txt")

  expect_equal(length(codebook), 45)
  expect_true(grepl("Interesse for deltagelse", codebook[1]))
  expect_true(grepl("Jeg har ikke vært med før", codebook[45]))
})

codebook <- codebook_read("../codebook-82087-2018-09-03.txt")

test_that("Does getting title and form id work", {
  expect_equal(codebook_title(codebook), "Interesse for deltagelse")
  expect_true(is.character(codebook_title(codebook)))

  expect_equal(codebook_id(codebook), 82087)
  expect_true(is.integer(codebook_id(codebook)))

})

test_that("Does cleaning the codebook work", {
  cleaned <- codebook_clean(codebook )

  expect_true(is_tibble(cleaned))
  expect_length(cleaned, 4)
  expect_equal(nrow(cleaned), 11)

  expect_equal(cleaned$column,
               c("Etternavn", "Bursdag", "Epost", "Telefon", "Adresse", "Sted", "Postnummer",
                 "TilleggsInfo", "Inklusjon", "Deltatt_tidligere", "Nyrekruttert"))

  expect_false(any(is.na(cleaned$question)))
  expect_true(any(is.na(cleaned$code)))

})



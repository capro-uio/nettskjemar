test_that("rm_ext works", {
  expect_equal(rm_ext("path.txt"), "path")
  expect_equal(rm_ext("some/path.txt"), "some/path")
  expect_equal(rm_ext("path.txt.txt"), "path.txt")
})

test_that("default to works", {
  expect_equal("a" %||% "b", "a")
  expect_equal(NA %||% "b", "b")
  expect_equal(NULL %||% "b", "b")
  expect_equal(list() %||% "b", "b")
})

dt <- lapply(1:10, function(x) {
  list(
    here = x,
    there = TRUE,
    everywhere = "a",
    nothing = NULL
  )
})

test_that("list2df works", {
  dt_ret <- list2df(dt)
  expect_is(dt_ret, "data.frame")
  expect_equal(nrow(dt_ret), 10)
  expect_equal(ncol(dt_ret), 4)
  expect_equal(dt_ret$nothing[1], NA)
})

test_that("list2row works", {
  dt_ret <- list2row(dt[[1]])
  expect_is(dt_ret, "data.frame")
  expect_equal(nrow(dt_ret), 1)
  expect_equal(ncol(dt_ret), 4)
  expect_equal(dt_ret$nothing[1], NA)
})

test_that("null2na works", {
  dt_ret <- null2na(dt[[1]])
  expect_is(dt_ret, "list")
  expect_length(dt_ret, 4)
  expect_equal(dt_ret$nothing, NA)
})

test_that("merge_el works", {
  df1 <- data.frame(element_no = c(1, 2), value = c("a", "b"))
  df2 <- data.frame(element_no = c(2, 3), value = c("c", "d"))
  result <- merge_el(df1, df2)
  expect_equal(nrow(result), 3)
  expect_equal(ncol(result), 3)
  expect_true(all(c("element_no", "value.x", "value.y") %in% colnames(result)))
})

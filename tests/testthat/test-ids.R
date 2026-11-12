test_that("file stems are safe", {
  expect_equal(pd_file_stem("P01-a.b_c"), "P01-a.b_c")
  expect_equal(pd_file_stem("../x y/æ"), "_._x_y__")
  expect_equal(pd_file_stem(".hidden"), "_hidden")
})

test_that("IDs are trimmed and checked", {
  expect_equal(pd_check_id("  P01 ")$id, "P01")
  expect_false(pd_check_id("   ")$ok)
  expect_false(pd_check_id(NULL)$ok)
  expect_false(pd_check_id(strrep("x", 101))$ok)
  expect_true(pd_check_id(strrep("x", 100))$ok)
})

test_that("an ID can't be reused for another drawing, but can for the same", {
  used <- c("1" = 1L, "a b" = 2L)
  expect_false(pd_check_id("1", used, drawing = 3L)$ok)
  expect_true(pd_check_id("1", used, drawing = 1L)$ok)
  # "a_b" gives the same file name as "a b"
  chk <- pd_check_id("a_b", used, drawing = 3L)
  expect_false(chk$ok)
  expect_match(chk$message, "same file name")
})

test_that("next ID increments the trailing number", {
  expect_equal(pd_next_id("1", "1"), "2")
  expect_equal(pd_next_id("1", c("1", "2")), "3")
  expect_equal(pd_next_id("P009", "P009"), "P010")
  expect_equal(pd_next_id("abc", "abc"), "abc_2")
  expect_equal(pd_next_id("7", character()), "7") # unused: keep
  expect_equal(pd_next_id("", character()), "1")
})

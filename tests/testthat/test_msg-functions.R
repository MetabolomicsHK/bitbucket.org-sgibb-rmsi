context("msg")


test_that("msg-functions", {
  expect_message(rmsi:::.msg(TRUE, "foobar"), "foobar")
  expect_message(rmsi:::.msg(TRUE, "foo", "bar"), "foobar")
  expect_that(rmsi:::.msg(FALSE, "foobar"), not(shows_message()))
})


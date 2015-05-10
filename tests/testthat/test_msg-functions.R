context("msg")


test_that("msg-functions", {
  expect_message(msir:::.msg(TRUE, "foobar"), "foobar")
  expect_message(msir:::.msg(TRUE, "foo", "bar"), "foobar")
  expect_that(msir:::.msg(FALSE, "foobar"), not(shows_message()))
})



# local({
test_that("test 1", {
  foo <- 1
  expect_equal(foo, 1)

  penv <- parent.env(environment())
  penv$foo <- foo
})

test_that("test 1", {
  bar <- foo + 1
  expect_equal(bar, 2)
})
# })

print(exists("foo")) # should not exists but it does
print(exists("bar"))

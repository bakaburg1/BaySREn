print(exists("foo")) # should not exists but it does

test_that("test 3", {
  bar <- foo + 2
  expect_equal(bar, 3)
})
# })

print(exists("bar"))

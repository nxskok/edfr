context("random stuff")

test_that("10 is equal to 10",{
  expect_equal(10,10)
})

test_that("square root of 2",{
  expect_equal(sqrt(2),1.41,tolerance=0.005)
}
)

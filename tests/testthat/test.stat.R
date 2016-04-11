context("Test statistics")

# compare test statistics and P-values with textbook

test_that("a2 with leghorn data", {
  w=p.val("a2",leghorn$x,nsim=1e4,sim=rnorm,calc=pnorm,200,35)
  expect_equal(w$test.stat,1.017,tolerance=0.001)
  expect_gt(w$p.value,0.25)
})

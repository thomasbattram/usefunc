test_that("is.binary() identifies binary vectors", {
    x <- rep(c("x", "y"), each = 10)
    y <- rep(c(100, 5), each = 10)
    z <- factor(c(1,2))
    x_bin <- is.binary(x)
    y_bin <- is.binary(y)
    z_bin <- is.binary(z)
    
    expect_equal(x_bin, TRUE)
    expect_equal(y_bin, TRUE)
    expect_equal(z_bin, TRUE)
})

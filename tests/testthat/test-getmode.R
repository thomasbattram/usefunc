test_that("getmode() gets most common number", {
	x <- c(10,10,11)
	getmode_x <- getmode(x)
	y <- c(10, 11)
	getmode_y <- getmode(y)
	z <- c("a", "b", "a")
	getmode_z <- getmode(z)

	expect_equal(getmode_x, 10)
	expect_equal(getmode_y, c(10, 11))
	expect_equal(getmode_z, "a")
})

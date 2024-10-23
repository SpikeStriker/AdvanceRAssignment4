data("iris")
library("MASS")

Polygon <- setRefClass("Polygon", fields = c("sides"))
square <- Polygon$new(sides = 4)
test_that("ridgereg rejects errounous input", {
  expect_error(ridgereg_mod <- ridgereg$new(formula = Petal.Length~Sepdsal.Width+Sepal.Length, data=iris, lambda=1))
  expect_error(ridgereg_mod <- ridgereg$new(formula = Petal.Length~Sepdsal.Width+Sepal.Length, data=irfsfdis, lambda=1))
  expect_error(ridgereg_mod <- ridgereg$new(formula = Petal.Length~Sepal.Width+Sepal.Length,data=iris, lambda="a"))
  expect_error(ridgereg_mod <- ridgereg$new(formula = Petal.Length~Sepal.Width+Sepal.Length,data=iris, lambda=1,qrDecomposition="a"))
})


test_that("class is correct", {
  ridgereg_mod <- ridgereg$new(formula = Petal.Length~Sepal.Width+Sepal.Length,data=iris, lambda=1)
  
  expect_true(class(ridgereg_mod)[1] == "ridgereg")
})

test_that("print() method works", {
  ridgereg_mod <- ridgereg$new(formula = Petal.Length~Sepal.Width+Sepal.Length,data=iris, lambda=1)

  expect_output(ridgereg_mod$print(),"ridgereg\\(formula = Petal\\.Length ~ Sepal\\.Width \\+ Sepal\\.Length, data = iris\\)")
  expect_output(ridgereg_mod$print(),"( )*\\(Intercept\\)( )*Sepal\\.Width( )*Sepal\\.Length")
})

test_that("predict() method works", {
  ridgereg_mod <- ridgereg$new(formula = Petal.Length~Sepal.Width+Sepal.Length,data=iris, lambda=1)
  ridgereg_MASSmod <- lm.ridge(formula = Petal.Length~Sepal.Width+Sepal.Length,data=iris, lambda=1)
  expect_equal(as.matrix(ridgereg_mod$predict()),as.matrix(cbind(const=1,iris[,c("Sepal.Width","Sepal.Length")])) %*% coef(ridgereg_MASSmod))

  ridgereg_mod <- ridgereg$new(formula = Petal.Length~Sepal.Width+Sepal.Length-1,data=iris, lambda=1)
  ridgereg_MASSmod <- lm.ridge(formula = Petal.Length~Sepal.Width+Sepal.Length-1,data=iris, lambda=1)
  expect_equal(as.matrix(ridgereg_mod$predict()),as.matrix(iris[,c("Sepal.Width","Sepal.Length")]) %*% coef(ridgereg_MASSmod))
  
  ridgereg_mod <- ridgereg$new(formula = Petal.Length~Sepal.Width+Sepal.Length,data=iris, lambda=1, qrDecomposition=TRUE)
  ridgereg_MASSmod <- lm.ridge(formula = Petal.Length~Sepal.Width+Sepal.Length,data=iris, lambda=1)
  expect_true(all.equal(as.matrix(ridgereg_mod$predict()),
                        as.matrix(cbind(const=1,iris[,c("Sepal.Width","Sepal.Length")])) %*% coef(ridgereg_MASSmod),tolerance=0.01))
})

test_that("coef() method works", {
  ridgereg_mod <- ridgereg$new(formula = Petal.Length~Sepal.Width+Sepal.Length,data=iris, lambda=1)
  ridgereg_MASSmod <- lm.ridge(formula = Petal.Length~Sepal.Width+Sepal.Length,data=iris, lambda=1)
  expect_true(all.equal(ridgereg_mod$coef(),ridgereg_MASSmod$coef))
  ridgereg_mod <- ridgereg$new(formula = Petal.Length~Sepal.Width+Sepal.Length-1,data=iris, lambda=1)
  ridgereg_MASSmod <- lm.ridge(formula = Petal.Length~Sepal.Width+Sepal.Length-1,data=iris, lambda=1)
  expect_true(all.equal(ridgereg_mod$coef(),ridgereg_MASSmod$coef))
  ridgereg_mod <- ridgereg$new(formula = Petal.Length~Sepal.Width+Sepal.Length,data=iris, lambda=1,qrDecomposition=TRUE)
  ridgereg_MASSmod <- lm.ridge(formula = Petal.Length~Sepal.Width+Sepal.Length,data=iris, lambda=1)
  expect_true(all.equal(ridgereg_mod$coef(),ridgereg_MASSmod$coef, tolerance=0.01))
})



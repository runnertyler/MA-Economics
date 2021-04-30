datasets::mtcars


model <- lm(mtcars$mpg ~ mtcars$wt)


with (mtcars, plot(wt, mpg))
abline(model)

average <- mean(mtcars$mpg)
average
plot(mtcars$mpg)
model
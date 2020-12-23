# Tidyverse helps keep your code understandable when transforming data.
library(tidyverse)
library(ggplot2)
library(caret)
install.packages("boot")
library(boot)
install.packages("ISLR")
library (ISLR)
# Read in historical global temperature data.
temp_dat = read.table('./Auto(2)(1).tsv')
names(temp_dat) <- as.matrix(temp_dat[1, ])
temp_dat <- temp_dat[-1, ]
temp_dat[] <- lapply(temp_dat, function(x) type.convert(as.character(x)))

set.seed (1)
train=sample(392,196)

lm_mod1 = glm(mpg~cylinders+horsepower+weight, data = temp_dat)
lm_mod2 = glm(mpg~cylinders+horsepower*weight, data = temp_dat)
lm_mod3 = glm(mpg~cylinders*horsepower+weight, data = temp_dat)
lm_mod4 = glm(mpg~cylinders*horsepower*weight, data = temp_dat)

cv1 = cv.glm(temp_dat, lm_mod1)
cv2 = cv.glm(temp_dat, lm_mod2)
cv3 = cv.glm(temp_dat, lm_mod3)
cv4 = cv.glm(temp_dat, lm_mod4)
cv1$delta
cv2$delta
cv3$delta
cv4$delta

#> cv1$delta
#[1] 18.10962 18.10918
#> cv2$delta
#[1] 15.64266 15.64217
#> cv3$delta
#[1] 15.39489 15.39438
#> cv4$delta
#[1] 15.52713 15.52632


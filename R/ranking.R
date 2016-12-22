library(data.table)
library(magrittr)

x <- mtcars %>% as.data.table(.)
x[, rank := frank(-hp, ties.method = c("dense")), by = .(carb)]
x[rank == 1]
x[rank == 2]


x <- mtcars %>% as.data.table(.)
x %>% setorder(-hp)
x[, rank := 1:.N, by = .(carb)]
x[rank == 1]
x[rank == 2]



x[, rank := frank(-hp, ties.method = c("dense")), by = .(am)]
x

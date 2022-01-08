library(grid)

plotLabyrinth <- function(lab)
{
	sel <- lab[,] == -1
	lab[sel] <- rgb(0, 0, 0)

	sel <- lab[,] == 0
	lab[sel] <- rgb(1, 1, 1)

	sel <- lab[,] == -2
	lab[sel] <- rgb(1, 0, 0)

	sel <- lab[,] == -3
	lab[sel] <- rgb(1, 1, 0)

	sel <- lab[,] == -4
	lab[sel] <- rgb(0, 1, 0)

	sel <- lab[,] == 1
	lab[sel] <- rgb(0.3, 0.3, 0.3)

	sel <- lab[,] == 2
	lab[sel] <- rgb(0.4, 0.4, 0.4)

	sel <- lab[,] == 3
	lab[sel] <- rgb(0.6, 0.6, 0.6)

	sel <- lab[,] == 4
	lab[sel] <- rgb(0.8, 0.8, 0.8)

	grid.newpage()
	grid.raster(lab, interpolate=F)
}

draw <- function(path)
{
  data <- read.table(path, sep=",", header=F)
  data <- as.matrix(data)
  screen <- plotLabyrinth(data)
}

draw("./seminar-2/data/labyrinth_9.txt")

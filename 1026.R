library(ggplot2)

g <- ggplot(data=mpg, mapping = aes(x=displ, y=cty))+geom_point()

g+coord_fixed(1)

g+coord_cartesian(ylim=c(20,35))

g+coord_polar(theta="x",direction = 1)


library(dplyr)


g+scale_color_discrete(name="transmission")


aa <- c(99,99,105,101,105,99)

ab <- as.factor(aa)
as.numeric(ab)
as.numeric((as.character(ab)))

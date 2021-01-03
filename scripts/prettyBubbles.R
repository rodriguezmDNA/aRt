# Libraries
library(ggplot2)
library(dplyr)

# The dataset is provided in the gapminder library


data <- iris

# Most basic bubble plot
data %>%
  mutate(country = factor(Species),
         SepArea=Sepal.Width*Sepal.Length,
         PetArea=Petal.Length*Petal.Width) %>%
  ggplot(aes(x=Sepal.Length, y=Sepal.Width, size=SepArea, color=Species)) +
  geom_point(alpha=0.25,show.legend = FALSE) +
  geom_point(alpha=0.25,aes(y=Petal.Length, x=Petal.Width, size=PetArea, color=Species),show.legend = FALSE) +
  theme_void()

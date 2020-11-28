## tidyverse needs openssl
## brew install openssl@1.1
#install.packages('tidyverse',dependencies = TRUE)

###
#install.packages('ambient')
#install.packages('scico')
#install.packages('here')

# Load the packages we need
library(tidyverse)
library(ambient) 
library(scico)
library(here)

## From Danielle Navarro
#https://colab.research.google.com/drive/1n2A4PNsuCHheMibli9K7lnyMXGkLG1ER#scrollTo=D7D0YGXdN3Cl


# Store the parameters that specify the piece in a list. In each case the 
# parameters should be a positive integer
art_par <- list(
  seed = sample(360,1),        # seed for the random number generator
  n_paths = 500,   # number of distinct paths to draw
  n_steps = 80,    # number of steps along each path
  sz_step = 200,   # what is the size of a typical step?
  sz_slip = 200     # what is the size of a typical slip?
)

state <- tibble(
  x = runif(art_par$n_paths, min = 0, max = 2), # uniform random number between 0 and 2
  y = runif(art_par$n_paths, min = 0, max = 2), # uniform random number between 0 and 2
  z = 0
)

state <- state %>% 
  mutate(
    path_id = 1:art_par$n_paths,
    step_id = 1
  )

art_dat <- state

stop_painting <- FALSE

while(stop_painting == FALSE) {
  
  # This is a little bit of magic... it takes the tibble corresponding to
  # the current "state" and generates a new "step" tibble with x, y and z
  # values that tell us how to move our brush...
  step <- curl_noise(
    generator = gen_simplex,
    x = state$x,
    y = state$y,
    z = state$z,
    seed = c(1, 1, 1) * art_par$seed
  )
  
  # Use the "step" data to mutate/modify the current state, moving the x and y
  # co-ordinates of the brush(es) and possibly "slipping" a little distance in
  # the z dimension...
  state <- state %>% 
    mutate(
      x = x + step$x * art_par$sz_step / 10000, # step along x
      y = y + step$y * art_par$sz_step / 10000, # step along y
      z = z + step$z * art_par$sz_slip / 10000, # step (invisibly) along z
      step_id = step_id + 1                     # increment the step number!
    )
  
  # Append the new data to the bottom of the art_dat tibble
  art_dat <- bind_rows(art_dat, state)
  
  # The value of step_id in the last row in art_dat tells us
  # how many steps we have taken so far
  current_step <- last(art_dat$step_id)
  
  # If we have taken as many steps as were specified in the
  # parameter list, then we should stop the painting!
  if(current_step >= art_par$n_steps) {
    stop_painting <- TRUE
  }
}


palette_name <- "corkO"

art_pic <- ggplot(
  data = art_dat,
  mapping = aes(
    x = x, 
    y = y, 
    group = path_id,
    color = step_id
  )
) + 
  geom_path(
    size = .5,
    alpha = .5,
    show.legend = FALSE
  ) +
  coord_equal() +
  theme_void() +
  scale_color_scico(palette = palette_name)



# print with ggplot ------------------------------------------------

pixels_wide <- 3000
pixels_high <- 3000

print(art_par$seed)
art_pic

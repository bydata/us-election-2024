library(tidyverse)
library(ggtext)

#' Sources:
#' Electoral College 2020: https://www.archives.gov/electoral-college/allocation
#' Voting population 2020: https://en.wikipedia.org/wiki/Voter_turnout_in_United_States_presidential_elections

ec <- read_tsv(file.path("data", "electoral-college-2020.tsv"))

ec |> 
  ggplot(aes(voting_population, electoral_votes)) +
  geom_point()

ec |> 
  mutate(
    ev_share = electoral_votes / sum(electoral_votes),
    vp_share = voting_population / sum(voting_population)
  ) |> 
  ggplot(aes(vp_share, ev_share)) +
  geom_point() +
  geom_abline() +
  scale_x_continuous(labels = scales::label_percent()) +
  scale_y_continuous(labels = scales::label_percent()) +
  coord_equal(xlim = c(0, 0.11), ylim = c(0, 0.11))

ec |> 
  mutate(
    ev_share = electoral_votes / sum(electoral_votes),
    vp_share = voting_population / sum(voting_population),
    vp_ev_ratio = voting_population / electoral_votes
  ) |> 
  ggplot(aes(vp_ev_ratio, fct_reorder(state, -vp_ev_ratio))) +
  geom_col() +
  scale_x_continuous(labels = scales::label_number())

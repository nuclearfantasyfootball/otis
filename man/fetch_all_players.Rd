% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sleeper_api_players.R
\name{fetch_all_players}
\alias{fetch_all_players}
\title{Fetch all NFL players from Sleeper API}
\usage{
fetch_all_players(save_path = "./data/raw/sleeper_players")
}
\arguments{
\item{save_path}{Character string specifying the directory path to save the JSON file}
}
\value{
No return value, called for side effect of saving player data to disk
}
\description{
Fetches complete NFL player data from the Sleeper API and saves it as a JSON file
}
\examples{
\dontrun{
path_save <- "./data/raw/sleeper_players"
fetch_all_players(path_save)
}
}

rm(list = ls())
library(tidyverse)

# revisions 12-4-2024 Jim Carretta to comment on correct columns
# for species and percentages. Most of this script was revised by 
# Eric Archer during Leg 2 of CalCurCEAS

# Merge DAS file with GroupSz file output, write group size events into DAS data.

# Event codes "A" signal line before group sizes should be entered.
# col 4-5 = event code and effort status
# col 6-11 = time
# col 13-18 = date
# event code, time, and date.
# col 41-44 = sighting number
# col 62-64 = species1
# col 67-69 = species2
# col 72-74 = species3
# col 77-79 = species4

### for group size lines:

# col 4 = sequential group size estimate (1 to the number of observers making an estimate)
# col 42-44 = observer code
# col 46-49 = best estimate
# col 51-54 = high estimate
# col 56-59 = low estimate
# col 62-64 = species1 percentage
# col 67-69 = species2 percentage
# col 72-74 = species3 percentage
# col 77-79 = species4 percentage

addObserverEstimates <- function(gs.csv, das) {
  # read and format csv file
  gs <- read.csv(gs.csv) |>
    setNames(c('sight.num', 'obs.num', 'best', 'high', 'low', 'sp1', 'sp2', 'sp3', 'sp4')) |>
    arrange(sight.num, obs.num) |>
    group_by(sight.num) |>
    mutate(est.num = 1:n()) |>
    ungroup() |>
    mutate(
      across(
        c(sp1, sp2, sp3, sp4), 
        \(x) ifelse(is.na(x), '', as.character(signif(x, digits = 2)))
      )
    ) |>
    select(sight.num, est.num, everything())
  # write fixed width formatted file
  gs.fwf <- tempfile()
  gdata::write.fwf(
    gs |>
      select(-sight.num) |>
      as.data.frame(),
    file = gs.fwf, 
    width = c(4, 39, rep(4, 7)), 
    colnames = FALSE,
    justify = 'right'
  )
  # read fixed width formatted file as character column to data frame
  gs$fwf <- readLines(gs.fwf)
  
  # Read DAS file and loop through lines
  old.das <- readLines(das, warn = FALSE)
  new.das <- paste0(das, '.groupsizes')
  no.estimates <- numeric()
  has.estimates <- numeric()
  for(i in 1:length(old.das)) {
    if(grepl("1|2|3|4|5|6|7", substr(old.das[i], 4, 4))) next
    write(old.das[i], new.das, append = i > 1)
    # check for estimate if this line is an A following an S
    if(substr(old.das[i - 1], 4, 4) == 'S' && substr(old.das[i], 4, 4) == "A") {
      this.sighting <- as.numeric(as.character(substr(old.das[i], 41, 44)))
      estimate.fwf <- gs |> 
        filter(sight.num == this.sighting) |>
        pull('fwf') 
      if(length(estimate.fwf) == 0) {
        # no estimate found
        no.estimates <- c(no.estimates, this.sighting)
      } else {
        # estimate found and written
        has.estimates <- c(has.estimates, this.sighting)
        write(estimate.fwf, new.das, append = TRUE)
      }
    }
  }
  
  # Output summary
  message('\nWrote new DAS file: ', new.das)
  message(
    '\nAdded estimates for sightings: ', 
    paste(has.estimates, collapse = ', ')
  )
  
  # Check mixed species percentage order
  mixed <- gs |>
    filter(sp2 != '') |>
    pull('sight.num') |>
    unique()
  if(length(mixed) > 0) message(
    '\nCheck for correct order of percentages in these mixed species sightings: ',
    paste(mixed, collapse = ', ')
  )
  
  # Sightings in DAS without estimates
  if(length(no.estimates) > 0) {
    message(
      '\nThe following sightings were in the DAS file, but had no estimates: ',
      paste(no.estimates, collapse = ', ')
    )
  }
  
  # Sightings in estimates but not in DAS
  no.sightings <- setdiff(gs$sight.num, has.estimates)
  if(length(no.sightings) > 0) {
    message(
      '\nThe following sightings had estimates but were not in the DAS file: ',
      paste(no.sightings, collapse = ', ')
    )
  }
}

# Apply the function Observer_Estimates to a DAS file
addObserverEstimates('GroupSizes_Test.csv', 'DAS_TEST.das')


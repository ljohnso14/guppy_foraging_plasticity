#' Process foraging data
#'
#' Read and process the foraging data from the raw file to
#' a .rda file.
#' @param data The data to process
#' @return An .rda dataframe
#' @export
#'
make_forage <- function(data = "foraging_data.csv"){

  # read data file
  temp <- read.csv(here::here(paste('data_raw', data, sep = '/')))

  exclude_NA <- temp %>%
    mutate(Tutor = factor(ifelse(Population == Tutor_pop, 'same', 'diff'))) %>%
    filter(Sex != 'dead') %>%
    filter(!(ID %in% c('ALF2_1O2O.2.6', 'AHF2_1O3Y.2.2', 'ALF2_1O2O.3.3', #removing fish that had were not tested for a trial
                       'ALF2_1P2Y.2.4', 'ALF2_1P2Y.2.8', 'ALF2_3G4G.1.4',
                       'ALF2_1P3G.4.4', 'ALF2_1O3Y.3.7', 'ALF2_1P4G.3.4')))

  # In this vignette we want to reanalyze the data excluding any data from runs 2 and 3,
  # and instead use data from every individual’s first run. What that means is that we
  # need to substitute the Attempts data for individuals with more than 1 run to be 0,
  # and Latency to be 600+

  data_forage <- exclude_NA %>%
    mutate(Attempts = ifelse(Run == 1, Attempts, 0),
           Latency = ifelse(Run == 1, Latency, 600),
           event = ifelse(Latency == 600, 0, 1),
           time2peck = Surv(Latency, event = event),
           Predator = Pred,
           Tutor = Tutor_pop)

  # save data
  save(data_forage, file = here::here('data/data_forage.Rda'))
}



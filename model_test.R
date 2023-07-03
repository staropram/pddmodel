source('config.R')
source('police_forces.R')
source('participants.R')

# test data
d <- list(pf=create_random_police_forces())
d$pd <- create_random_participants(d$pf)

# explore available accelerometry data in NHATS
# written by shelby bachman, shelby.bachman@vivosense.com


# setup -------------------------------------------------------------------

library(here)
library(haven)
library(dplyr)


# read data ---------------------------------------------------------------

data_accel_summ <- read_sas(here('data', 'nhats_R11',
                                 'NHATS_Round_11_Accel_Summ_File.sas7bdat'))

data_accel_track <- read_sas(here('data', 'nhats_R11',
                                  'NHATS_Round_11_Accel_Track_File.sas7bdat'))

data_accel_det <- read_sas(here('data', 'nhats_R11',
                                'NHATS_Round_11_Accel_Det_File.sas7bdat'))

data_SP <- read_sas(here('data', 'nhats_R11',
                         'NHATS_Round_11_SP_File.sas7bdat'))


# relevant metadata -------------------------------------------------------

# number of participants with acc data
n_participants <- data_accel_det$spid %>% unique() %>% length()

# number of days with 21.6+ hours wear time
mean_validdays <- data_accel_summ$ag11dnumdaysval %>% mean()
sd_validdays <- data_accel_summ$ag11dnumdaysval %>% sd()
min_validdays <- data_accel_summ$ag11dnumdaysval %>% min()
max_validdays <- data_accel_summ$ag11dnumdaysval %>% max()

# number of participants with >= 4 days of 21.6+ hours wear time
n_atleast4valid <- sum(data_accel_summ$ag11dnumdaysval)


# demographics ------------------------------------------------------------

# bind relevant demographic variables
data_accel_summ <- data_accel_summ %>%
  left_join(by = 'spid',
            data_SP %>%
              select(spid,
                     r5dgender,
                     rl5dracehisp,
                     r11d2intvrage)) %>%
  # convert demographic variables from numeric to string
  rowwise() %>%
  mutate(
    gender = case_when(
      r5dgender == 1 ~ 'Male',
      r5dgender == 2 ~ 'Female'
      ),
    race_ethnicity = case_when(
      rl5dracehisp == 1 ~ 'White, non-hispanic',
      rl5dracehisp == 2 ~ 'Black, non-hispanic',
      rl5dracehisp == 3 ~ 'Other, non-hispanic',
      rl5dracehisp == 4 ~ 'Hispanic',
      rl5dracehisp == 5 ~ 'More than one DKRF primary',
      rl5dracehisp == 6 ~ 'DKRF'
      ),
    age_range = case_when(
      r11d2intvrage == -1 ~ 'Inapplicable',
      r11d2intvrage == 2 ~ '70-74',
      r11d2intvrage == 3 ~ '75-79',
      r11d2intvrage == 4 ~ '80-84',
      r11d2intvrage == 5 ~ '85-89',
      r11d2intvrage == 6 ~ '90+'
    )
  )

# summarize demographic variables
summary_demographics <- as.data.frame(
  rbind(
    data_accel_summ %>%
      group_by(age_range) %>%
      summarize(n = n()) %>%
      select(category = age_range, n),
    data_accel_summ %>%
      group_by(gender) %>%
      summarize(n = n()) %>%
      select(category = gender, n),
    data_accel_summ %>%
      group_by(race_ethnicity) %>%
      summarize(n = n()) %>%
      arrange(desc(n)) %>%
      select(category = race_ethnicity, n)
  )
) %>%
  mutate(variable = NA, .before = category)

summary_demographics$variable[1] <- 'Age'
summary_demographics$variable[6] <- 'Gender'
summary_demographics$variable[8] <- 'Race/ethnicity'


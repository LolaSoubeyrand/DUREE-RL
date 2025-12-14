library(dplyr)
full_data <- read.csv("data/data_fourchoices.csv")

# mapping of button names to choice numbers
button_mapping <- c(
    "antifragile" = 1,
    "robuste" = 2,
    "fragile" = 3,
    "vulnerable" = 4
)

data <- full_data[, c("participant", "click_number", "button_name", "button_value")] %>%
    rename(participant_id = participant, trial = click_number, choice = button_name, reward = button_value) %>%
    mutate(
        option = choice,
        choice = button_mapping[choice]
    ) %>%
    select(participant_id, trial, choice, reward, option) -> data

write.csv(data, file = "data/prepared_data.csv", row.names = FALSE)

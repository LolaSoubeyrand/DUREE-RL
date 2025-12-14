# %%
import pandas as pd

full_data = pd.read_csv("data/data_fourchoices.csv")

# %%
all_participant_data = full_data[["participant", "button_name", "button_value"]]
# %%
dict_mapping_button_name_to_index = {
    "antifragile": 0,
    "fragile": 1,
    "robuste": 2,
    "vulnerable": 3
    }
all_participant_data["choice"] = all_participant_data["button_name"].map(dict_mapping_button_name_to_index)
all_participant_data = all_participant_data.rename(columns={"button_value": "reward"})

all_participant_data["rescaled_reward"] = (all_participant_data["reward"]) / 3000 # rescale rewards to [-1,1]
all_participant_data["rescaled_reward"] = all_participant_data["rescaled_reward"].round(4)

unique_participants = all_participant_data["participant"].unique()
# %%

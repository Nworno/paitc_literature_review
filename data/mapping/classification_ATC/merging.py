import pandas as pd 
import numpy as np
import os


concept_df = pd.read_csv("CONCEPT.csv", sep="\t")\
    .assign(simplified = lambda df: df["concept_name"].str.partition(sep = ";")[0].str.strip())
ttt_names = pd.read_csv("ttt_names.csv")
ttt_names_joined = ttt_names.join(\
    concept_df.set_index("simplified").loc[:, ["concept_class_id", "concept_code"]], 
    how="left", 
    on="simplified")

ttt_names_joined["ATC code"] = np.where(ttt_names_joined["ATC code"].isna(), ttt_names_joined["concept_code"], ttt_names_joined["ATC code"])
ttt_names_joined["ATC code"] = ttt_names_joined["ATC code"].replace("{n/a}", value = np.NaN, regex = False)

ttt_names_joined = ttt_names_joined.drop_duplicates(subset = ["ttt_name"])
ttt_names_joined.to_csv("joined.csv")
import pandas as pd
import matplotlib.pyplot as plt
import os

#Load data
df = pd.read_csv("Shoreline-Access-Pts_v2-1-attribute-table.csv") 

#Aggregate People of Color served by access mode
poc_by_mode = (
    df.groupby("Service_Type")["SUM_Estimated_People_of_Color"]
      .sum()
      .reset_index()
)

#Create bar chart
plt.figure(figsize=(8, 5))
plt.bar(
    poc_by_mode["Service_Type"],
    poc_by_mode["SUM_Estimated_People_of_Color"]
)

plt.xlabel("Access Mode")
plt.ylabel("Estimated People of Color Served")
plt.title("People of Color Served by Shoreline Access Mode")
plt.tight_layout()
plt.savefig("poc_served_by_access_mode.png", dpi=300, bbox_inches="tight")
plt.show()

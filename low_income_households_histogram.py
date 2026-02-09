import pandas as pd
import matplotlib.pyplot as plt
import os

# Load data
df = pd.read_csv("Shoreline-Access-Pts_v2-1-attribute-table.csv")

low_income_vals = df["SUM_Estimated_Low_Income_Households"]

print(f"Number of access-point rows used: {len(low_income_vals)}")

# Create histogram
plt.figure(figsize=(8, 5))
plt.hist(
    low_income_vals,
    bins=30,
    edgecolor="black"
)

plt.xlabel("Estimated Low-Income Households Served per Access Point")
plt.ylabel("Number of Access Points")
plt.title("Distribution of Low-Income Households Served by Shoreline Access Points")
plt.tight_layout()
plt.savefig("low_income_households_per_access_point.png", dpi=300, bbox_inches="tight")
plt.show()

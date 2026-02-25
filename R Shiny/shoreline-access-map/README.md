## SF Bay Shoreline Access: Priority Target Regions

[Open the dashboard](https://jeffreyd.shinyapps.io/shoreline-access-map/)

**Description:** 
This prototype dashboard aggregates shoreline access points into a hexagonal grid and 
 attempts to quantify the priority target regions for improved shoreline access. 
Each hex contains the access points that fall within its boundary. 
We then summarize the demographic features associated with those points and compute a **priority score** per hex based on user-chosen weights.

**Percent vs Count:**  
- **Percent mode:** Ranks hexes based on the share of people or households served that fall into each group (e.g., percentage of households served that are low-income).  
- **Count mode:** Ranks hexes based on raw totals (e.g., number of low-income households served).

**Priority Score:**  
For each hex we compute three underlying metrics: people of color, low-income status, and vehicle ownership.  
Depending on the selected mode, these metrics are either percentages or counts.

1. **Compute metric values per hex:** Sums are taken across all access points inside each hex. Percent metrics are computed as:  
   - `poc_pct = poc_pop / total_pop`  
   - `lowinc_pct = lowinc_households / total_households`  
   - `noveh_pct = noveh_households / total_households`

2. **Normalize each metric to a 0–1 scale:** Since counts and percentages can be on different scales, each metric is normalized using the minimum and maximum across all hexes in the current view:  
   - `z = (x - min(x)) / (max(x) - min(x))`  
   If a metric has no variation (i.e., `min = max`), it contributes 0 everywhere.

3. **Weight and combine:** The final score is a weighted sum of the normalized metrics (z-scores):  
   - `score = weight_poc*z_poc + weight_lowinc*z_lowinc + weight_noveh*z_noveh`  
   If all weights are set to 0, scores default to 0 (no rankings given).

4. **Rank:** Hexes are ranked by priority score in descending order (highest score = rank 1).

**Other Features + Interpreting:**  
- Higher score (lighter color hexes) means higher priority under the current mode and weight inputs.  
- The **top N hexes** input draws a red outline around the highest-ranked hexes.  
- The **hex size** input changes the analysis resolution (larger hex size reduces the number of hexes).

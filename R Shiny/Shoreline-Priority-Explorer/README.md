## SF Bay Shoreline Access: Equitable Access Priority Regions

[Open the dashboard](https://jeffreyd.shinyapps.io/Shoreline-Priority-Explore/)

---

### How the App Works

This app maps **BCDC block groups** and assigns each one a **priority score** based on two components:

1. **Demographic burden**
2. **Nearby shoreline access conditions**

Block groups with higher scores appear darker on the map.
&emsp;

**1. Demographic Burden**
Users can select one or more demographic indicators, such as:

- **Households Without a Vehicle**
- **People of Color**
- **Limited English Proficiency**

Each selected indicator has a **weight slider** from 0 to 1.

For each block group, the app takes the corresponding **percentage estimate** from the BCDC dataset, rescales it relative to all other block groups, and adds it into the demographic score using the selected weight. This means that block groups with higher values on the selected burden indicators receive a higher **demographic score**.
&emsp;

**2. Nearby Shoreline Access**

The app also summarizes shoreline access near each block group. It does this by:

- taking the **centroid** of each block group
- drawing a search radius around that centroid using the selected distance in kilometers
- identifying all shoreline access points within that radius

From those nearby access points, the app calculates shoreline metrics such as:

- Number of shoreline access points
- Share with walk access
- Share with bike access
- Share with drive access
- Nearby transit stops
- Nearby transit route count
- Mean trail quality

Users can choose which shoreline metrics to include and assign each one a weight. These shoreline metrics are treated as **deficit measures**, meaning that lower access corresponds to higher priority. For example:

- fewer access points = higher priority
- lower transit availability = higher priority
- lower trail quality = higher priority

These weighted deficit measures are combined into the **shoreline score**.

---

### Final Priority Score

The app computes the final **priority score** as:

**Priority Score = Demographic Score + Shoreline Score**

A block group will therefore rank higher if it has either/both of:

- greater demographic burden
- poorer nearby shoreline access

---

## Other App Functionalities

**1. Apply Shoreline Settings Button**

To improve app loading time and efficiency, the app is designed so that demographic controls update immediately, but shoreline controls only update when the user clicks _Apply shoreline settings_. This includes:

- selected shoreline metrics
- shoreline metric weights
- shoreline access radius

This prevents the app from recomputing nearby shoreline summaries every time the user adjusts a shoreline-related control, which improves performance. Note that this means the button must be pressed once to initalize the app.
&emsp;

**2. Reliability Outlines**

Some demographic indicators have high margins of error. If any selected demographic estimate has:

**MOE / Estimate > 0.5**

the block group is outlined in **red**. This outline is a warning that at least one selected demographic input is relatively unreliable and should be interpreted with caution.
&emsp;

**3. Interactive Features**

- Checking **Show shoreline access points** overlays the shoreline access point locations on the map.
- Clicking a block group opens a detail panel showing:
  - its priority score
  - demographic inputs
  - shoreline access summary
  - reliability information

---

### How to Interpret the Map

This app is best understood as a **relative prioritization tool**. It helps answer questions like:

> Which block groups appear highest priority under the demographic and shoreline access criteria currently selected?

The score is most useful for:

- comparing block groups to one another
- testing how priorities change under different weighting choices
- exploring how different definitions of access and equity shift the map

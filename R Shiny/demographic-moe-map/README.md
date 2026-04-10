## SF Bay Shoreline Access: Demographic / Social Vulnerability Data Reliability Explorer

[Open the dashboard](https://jeffreyd.shinyapps.io/demographic-moe-map/)


**Description:**
Uses [BCDC 2023 Community Vulnerability data](https://gis.data.ca.gov/datasets/BCDC::community-vulnerability-bcdc-2023/about)
 to visualize the margins of error and reliability of the demographic data used in the
 [**Priority Target Regions** interactive map](https://jeffreyd.shinyapps.io/shoreline-access-map/). 

The original data provides proportion estimates along with respective margins of error for various social vulnerability indicators. 
The data reliability explorer shows the **ratio of margin of error / estimate** for a selected indicator for each census block group.
Lower ratios indicate more reliable data. Census block groups with a margin of error greater than 50% of the estimate are outlined
 and treated as **unreliable**.

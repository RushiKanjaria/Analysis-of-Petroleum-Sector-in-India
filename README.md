**Introduction**

The petroleum sector in India has witnessed significant changes over the past few decades, transforming the energy landscape of the nation. As one of the world's top energy consumers, India's relationship with petroleum products is multi-faceted, influencing both the domestic economy and international relations. This report outlines and justifies the decisions made in the process of creating the visualisation of this sector.

**Overview**

The project was created using RStudio , with the Shiny package for interactive web application. The data was sourced from the Petroleum Planning and Analysis Cell (PPAC) of the Government of India. The data wrangling process was performed in R Markdown file using various R packages such as tidyverse, dplyr, and janitor. The visualisations were created using ggplot2, plotly, highcharter, and leaflet. The shinydashboard package and HTML tags were used to create a dashboard layout for the Shiny app.

**Dataset**

The datasets are timeseries data which contains records of India’s petroleum sector since 1998, which includes the quantity of import/export of crude oil and petroleum products in 1000 metric tons. Production of petroleum products by refining crude oil in 1000 metric tons: for e.g.- LPG, Motor Spirit, High Speed Diesel, etc., domestic consumption of petroleum products in 1000 metric tons, state wise annual sales of motor spirit in 1000 metric tons, and state wise annual sales of high-speed diesel in 1000 metric tons.

We have another dataset which contains the shape file of India. Shape file is used to create a visualisation of geographical area. Shape file consists of nontopological geometry and attribute information for spatial features in a data set.

**Design Principles and Visualisation**

The design principles discussed in class were applied to ensure the visualisations were clear, concise, and effectively communicated the data.

The design of every visualisation is kept simple and uncluttered. Consistent color schemes were used across all visualisations to create an aesthetic and appealing look. Interactive elements were integrated to allow user to explore the data. All the visualisations were made accessible, with clear labels, legend, and hovering tooltips to provide user-friendly environment.

The Shiny app includes several visualisations, each providing different insights into the petroleum sector in India.

• Line Graphs: Given the datasets are timeseries data, and since line graphs are drawn to show information that changes over time. I have used line graph to shows the trends in the import and export of petroleum products from 1998 to 2022.

• Pie Chart: The dataset consists of various categories which are called petroleum products, and to analyze the proportion of import/export of different petroleum products for any selected year, I have used pie chart. Since pie charts are drawn to illustrate numerical proportions.

• Sunburst Chart: The dataset can be observed in a hierarchical form, for instance – we have a year let’s say 2019, a year has twelve months, in these twelve months import and export of various petroleum products are taking place. So, this can be observed as a hierarchy, we have year, months, products, and quantity of import and export. Since, sunburst chart is used to Visualise hierarchical data, we can transform our dataset into hierarchical form using R library “highcharter” and plot the sunburst plot to Visualise the quantity of import and export of various petroleum products over the twelve months for any selected year.

• Choropleth Map: The dataset also consists the quantity of sales of motor-spirit and high-speed-diesel in different states of India and we have the shape file of India. Since, a choropleth maps provide an easy way to Visualise how a variable varies across a geographic area. I have used “leaflet” library of R to plot two choropleth maps, to Visualise the sales of MS and HSD across different states of India for any selected year.

**Conclusion**

The visualisations provide a comprehensive overview of the petroleum sector in India, revealing trends and patterns in production, consumption, imports, exports, and sales. They highlight the sector's growth and evolution, as well as the challenges it faces, such as dependency on imports and regional disparities in consumption. The Shiny app allows users to interact with the data and gain deeper insights, making it a valuable tool for understanding the petroleum sector in India.

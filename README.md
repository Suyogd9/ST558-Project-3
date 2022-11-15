## Project Summary

The purpose of this repo site is to showcase the process and results of automating Markdown reports containing predictive analysis about shares from articles published by Mashable. There are six categories of articles within the data, each of have their own report. They can be accessed via the links below, under _Generated Reports_.

For the code to perform as expected, the following R packages were used and should be installed:
- [Tidyverse](https://www.tidyverse.org/) - For intuitive data manipulation and analysis; also enables use of its sub-packages, the following of which were used:
   - [ggplot2](https://ggplot2.tidyverse.org/), [readr](https://readr.tidyverse.org/), [dplyr](https://dplyr.tidyverse.org/), [tibble](https://tibble.tidyverse.org/), and [tidyr](https://tidyr.tidyverse.org/)
- [caret](https://github.com/topepo/caret/) - For training and plotting models for classification and regression problems
- [leaps](https://www.rdocumentation.org/packages/leaps/versions/3.1/topics/leaps) - For selection of the best subset of predictor variables 
- [gbm](https://github.com/gbm-developers/gbm#readme) - Needed for `caret` training to recognize `gbm` method
- [ggcorrplot](https://cran.r-project.org/web/packages/ggcorrplot/readme/README.html) - Need to plot correlation plots with customizations

Generated Reports:
- [Lifestyle](LifestyleAnalysis.html)
- [Entertainment](EntertainmentAnalysis.html)
- [Business](BusinessAnalysis.html)
- [Social Media](Social_MediaAnalysis.html)
- [Technology](TechnologyAnalysis.html)
- [World](WorldAnalysis.html)

[Render_Script.R](Render_Script.R) - Generates report(s) based on provided value of `channel`

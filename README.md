# Clinical Trial Representation

[![License](https://img.shields.io/badge/License-Apache2-blue.svg)](https://www.apache.org/licenses/LICENSE-2.0) 


Quantify representativeness in randomized clinical trials and provide insights to improve the clinical trial equity and health equity

<!-- TABLE OF CONTENTS -->
## Contents

1. [Description](#description)
2. [Visualization Tool](#visualization-tool)
3. [Example RCTs](#example-rcts)
4. [Sample Visualization](#RCT-Representativeness-Visualization-Supplement-2021)
5. [Statistical Results](#statistical-results)
6. [Data Availability](#data-availability)
7. [Contact](#contact)
8. [License](#license)
9. [Acknowledgments](#acknowledgments)

<!-- ABOUT THE PROJECT -->
## Description
We develop randomized clinical trial (RCT) representativeness metrics based on Machine Learning (ML) Fairness Research. Visualizations and statistical tests based on proposed metrics enable researchers and physicians to rapidly visualize and assess subgroup representation in RCTs. The approach enables users to determine underrepresentation, absence, or other misrepresentation of subgroups indicating potential limitations of RCTs. The method could help support generalizability evaluation of existing RCT cohorts, enrollment target decisions for new RCTs (if eligibility criteria are included), and monitoring of RCT enrollment, ultimately contributing to more equitable public health outcomes.

### What's the problem?
Within the field of RCT research, there has been ongoing concern that RCTs which lack a diversity of participants may not provide clear evidence of efficacy and safety for new interventions in underrepresented or missing subpopulations. Standardized methods are needed to assess potential representation disparities between RCT cohorts and the broader populations who could benefit from novel interventions.


### How can technology help?
Extensive research in ML Fairness has created metrics for quantifying inequities in trained ML classification models and for creating novel ML approaches to address these inequities.

Our novel insight is that sampling of a subject to an RCT can be regarded as a classification function that is random. Applying ML Fairness metrics to this classification problem creates novel representativeness metrics for RCTs and other clinical studies. The representativeness metrics capture how well the actual sampling of subjects to a RCT matches of a hypothetical random sampling.


### The solution
We compare the observed rate in the RCT for the subgroup to the hypothetical ideal rate in an equitable RCT in which participants are assigned truly randomly to the clinical trial.  By considering assignment to the clinical trial as a random classification function, we develop standardized metrics based on variations of ML fairness metrics, focusing here on "Log Disaparity." The resulting metrics are functions of disease-specific observed and ideal rates of sampling of protected subgroups to the RCT. 

## Visualization Tool

You can run the example studies on [**RCT Representativeness Visualization (Paper Supplement)**](https://miao-qi-rpi-app.shinyapps.io/EquityBrowser/).

The tool can
1. Measure representativeness of any subgroup of interest in randomized clinical trials
2. Visualize representation for subgroups
3. Compare representation within/among studies

The R codes are available in the folder **Visualization Codes**.

### Prerequisites

What things you need to install the software and how to install them:
1. A software/server that can run R shiny codes
2. R packages used in the codes (available in *Source.R*)


## Example RCTs
We apply the proposed RCT representativeness metrics to three landmark clinical trials released in the last decade: Action to Control Cardiovascular Risk in Diabetes (**ACCORD**), Antihypertensive and Lipid-Lowering Treatment to Prevent Heart Attack Trial(**ALLHAT**), and Systolic Blood Pressure Intervention Trial (**SPRINT**). All participant data are obtained through the [Biologic Specimen and Data Repositories Information Coordinating Center (BioLINCC)](https://biolincc.nhlbi.nih.gov/home/).

## RCT Representativeness Visualization (Paper Supplement)

<p align="center">
  <img width="50%" height="auto" src="Visualizations/distribution_age_race.JPG">
</p>
<p align="center">
<em>The distributions of subgroups defined over age and race/ethnicity in both target population and ACCORD</em>
</p>

It demonstrates the distributions of participants from different age and race/ethnicity groups in the RCT ACCORD and the target population. This figure clearly identifies that young participants are missing from the clinical trial. Also, the higher red bin shows that the subgroup may be overrepresented in the RCT (e.g. non-Hispanic white subjects age 45-64), while the higher green bin shows that the subgroup has the potential to be underrepresented in the RCT (e.g. Hispanic participants age 45-64). The wider green bin means that the subgroup is missing from the clinical trial (e.g. non-Hispanic white participants age 18-44).


<p align="center">
  <img width="50%" height="auto" src="Visualizations/color_description.JPG">
</p>
<p align="center">
<em>Color representation of representativeness levels</em>
</p>

In our visualization, dark gray indicates that no people with selected protected attributes exist in the target population; light gray means that the subgroup is missing from both target population and study cohort; dark red represents the absent subgroup from the cohort; light orange and orange point out that some subgroups are not sufficiently represented and may be at risk of being insufficiently enrolled into and represented in the clinical trial cohort; on the other hand, light blue and blue identify the potential advantaged subgroups which may make inefficient treatment seem helpful or vice versa; teal shows that the subgroup is equitably represented in the clinical trial. 

<p align="center">
  <img width="50%" height="auto" src="Visualizations/color_equity.JPG">
</p>
<p align="center">
<em>Color representation with corresponding metric values</em>
</p>

The categorized metric values for different representativeness levels with corresponding color in the visualization is shown above. 


<p align="center">
  <img width="50%" height="auto" src="Visualizations/interactive_1.png">
</p>
<p align="center">
<em>The Log Disparity representativeness levels of subgroups defined over race/ethnicity, gender, age, and education and the corresponding function of observed rate for female subjects from other races with some college/technical school education and age over 64 in ACCORD, with significance level = 0.05,lower metric threshold = 0.2, and upper metric threshold = 0.4.</em>
</p>

This figure presents the representativeness levels of ACCORD subgroups defined by race/ethnicity, gender, age, and education from the inner ring to the outer ring using the Log Disparity metric. By hovering the pointer over the target subgroup areas on the sunburst, the representativeness label, ideal rate, and observed rate of the subgroups will show on the screen. Additionally, the corresponding math function of observed rates will also show on the side. The green line indicates the ideal rate and the brown line indicates the current RCT observed rate on the figure.

<p align="center">
  <img width="50%" height="auto" src="Visualizations/interactive_2.png">
</p>
<p align="center">
<em>The Normalized Parity representativeness levels of subgroups defined over race/ethnicity, gender, age, and education and the corresponding function of observed rate for female subjects from other races with some college/technical school education and age over 64 in ACCORD, with significance level = 0.05,lower metric threshold = 0.1, and upper metric threshold = 0.2.</em>
</p>

This figure presents the representativeness levels of ACCORD subgroups defined by race/ethnicity, gender, age, and education from the inner ring to the outer ring using the Normalized Parity metric. 



## Statistical Results
Subgroup data of ACCORD, ALLHAT, and SPRINT are summarised with the subgroups' charactersitics, observed rates in the RCT, ideal rates, Log Disparity value, group size, p-value, and BH p-value. 

The results are available in the folder **Statistical Results for Example Studies** as csv files.


## Data Availability 
Our dataset "Quantifying representativeness in RCTs using ML fairness metrics - Data and codes" are available in the Dryad Digital Repository at https://doi.org/10.5061/dryad.76hdr7sxf. 

The software is being published at Zenodo: https://doi.org/10.5281/zenodo.5348584.

The supplemental information is being published at Zenodo: https://doi.org/10.5281/zenodo.5348586.

<!-- CONTACT -->
## Contact

Miao Qi  - qim@rpi.edu

Project GitHub Link: [ClinicalTrialEquity](https://github.com/TheRensselaerIDEA/ClinicalTrialEquity)

## License

This project is licensed under the Apache 2 License

## Acknowledgments

* This work was primarily funded by IBM Research AI Horizons Network with the Rensselaer Institute for Data Exploration and Applications (IDEA)

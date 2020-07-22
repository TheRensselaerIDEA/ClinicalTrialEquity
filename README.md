# Clinical Trial Inequities

[![License](https://img.shields.io/badge/License-Apache2-blue.svg)](https://www.apache.org/licenses/LICENSE-2.0) 


Quantify the inequities in existing clinical trials and provide remedies to improve the clinical trial equity and health equity

## Contents

1. [Description](#description)
1. [Visualization Tool](#tool)
1. [Example_RCTs](#example_rcts)
1. [Sample Visualization](#sample-visualization)
1. [Statistical Results](#statistical-results)
1. [Authors](#authors)
1. [License](#license)
1. [Acknowledgments](#acknowledgments)


## Description
We develop randomized clinical trial (RCT) inequity metrics based on Machine Learning Fairness Research. Visualizations and statistical tests based on proposed metrics enable researchers and physicians to rapidly visualize and assess potential inequities in RCTs for subgroups. The approach enables users to determine underrepresentation of subgroups indicating potential limitations of RCTs. The method could help support evaluation of existing RCTs, design of new RCTs, monitoring of RCT recruitment, and applicability of RCT results to patients.

### What's the problem?
Within the field of randomized clinical trial (RCT) research, there has been ongoing concern that RCTs that lack a diversity of participants may not provide clear evidence of efficacy and safety for new interventions in underrepresented or missing sub-populations. To date, there has been no efficient means to quantify the inequity between those who get enrolled into RCTs and the broader population who could benefit from the new intervention.


### How can technology help?
Extensive  research  in  machine  learning  (ML)  fairness  has  created  metrics  for  quantifying  inequities  in  trained  MLclassification models and for creating novel ML approaches to address these inequities.

Our novel insight is that assignment of a subject to a RCT can be regarded as a classification function that is random. Applying ML-fairness metrics to this classification problem creates novel inequity metrics for RCTs and other clinical studies.The inequity metrics capture how well the actual assignment of subjects to a RCT matches of a hypothetical random assignment.


### The solution
We compare the observed rate in the RCT for the subgroup to the hypothetical ideal rate in an equitable RCT in which patients are assigned truly randomly to the clinical trial.  By considering assignment to the clinical trial as a random classification function, we develop standardized metrics based on variations of ML fairness metrics, focusing here on "Disparate Impact." The resulting metrics are functions of disease-specific observed and ideal rates of assignment of protected subgroups to the RCT. 

## Visualization Tool

You can run the example studies on [Equity Visualization Tool](https://miao-qi-rpi-app.shinyapps.io/EquityBrowser/).

1. Measure inequity in randomized clinical trials
2. Visualize inequity for subgroups
3. Compare inequities among studies

The R codes are available in the folder Visualization Codes.

### Prerequisites

What things you need to install the software and how to install them:
1. A software/server that can run R shiny codes


## Example RCTs
We apply the proposed RCT equity metrics to three landmark clinical trials released in the last decade: Action to ControlCardiovascular Risk in Diabetes (ACCORD), Antihypertensive and Lipid-Lowering Treatment to Prevent Heart AttackTrial(ALLHAT), and Systolic Blood Pressure Intervention Trial (SPRINT). All patient data are obtained through the [BiologicSpecimen and Data Repositories Information Coordinating Center (BioLINCC)](https://biolincc.nhlbi.nih.gov/home/).

## Sample Visualization
<p align="center">
  <img width="50%" height="auto" src="distribution_age_race.jpg">
</p>
<p align="center">
The distributions of subgroups defined over age and race/ethnicity in both target population and ACCORD.
</p>

It demonstrates the distributions of patients from different age and race/ethnicity groups in the RCT ACCORD and the target population. This figure clearly identifies that young patients are missing from the RCT. Also, the higher green bin shows that the subgroup may be overrepresented in the RCT (e.g. Non-Hispanic white subjects age 45-64), while the higher red bin shows that the subgroup has the potential to be underrepresented in the RCT (e.g. Hispanic participants age 45-64). 


<p align="center">
  <img width="50%" height="auto" src="color_description.jpg">
</p>
<p align="center">
Color representation of inequity levels.
</p>

In our visualization, grey indicates that no people with selected protected attributes exist in NHANES; black means that the subgroup is missing in both NHANES and RCT; dark red represents the absent subgroup from the RCT; orange and light orange point out that some subgroups are not sufficiently represented and may be at risk of being treated inappropriately during the RCT; on the other hand, blue and light blue specify the potential advantaged subgroups which may make inefficient treatment seem helpful or vice versa; white shows that the subgroup is equitably represented in the RCT. 

<p align="center">
  <img width="50%" height="auto" src="interactive_1.jpg">
</p>
<p align="center">
The Log Disparate Impact equity levels of subgroups defined over race/ethnicity, gender, education, and age and the corresponding function of observed rate for subgroup Non-Hispanic White Female with some college/technical school education and age over 64 in ACCORD, with significance level = 0.05,lower equity threshold = 0.2, and upper equity threshold = 0.4.
</p>

This figure presents the equity levels of subgroups defined by race/ethnicity, gender, education, and age from the inner ring to the outer ring using the Log Disparate Impact metric. By hovering the pointer over the target subgroup ares on the sunburst, the equity label, ideal rate, and observed rate of the subgroups will show on the screen. Additionally, the corresponding math function of observed rates will also show on the side. The green line indicates the ideal rate and the brown line indicates the current RCT observed rate on the figure.


<p align="center">
  <img width="50%" height="auto" src="interactive_2.jpg">
</p>
<p align="center">
The Quality Metric equity levels of subgroups defined over race/ethnicity, gender, education, and age and the corresponding function of observed rate for subgroup Non-Hispanic White Female with some college/technical school education and age over 64 in ACCORD, with significance level = 0.05,lower equity threshold = 0.2, and upper equity threshold = 0.4.
</p>

For the same subgroups, we apply the Quality Metric, Adjusted Equal Opportunity, and Normalized Mutual Information metrics separately. This is an example visualization using 
the Quality Metric on the subgroup Non-Hispanic White Female with some college/technical school education and age over 64 in ACCORD. 


## Statistical Results
Subgroup data of ACCORD, ALLHAT, and SPRINT are summarised with the subgroups' charactersitics, observed rates in the RCT, ideal rates, Log Disparate Impact equity value, group size, p-value, and BH p-value. 

The results are available in the folder Statistical Results for Example Studies as csv files.

## Authors

* **Miao Qi** 

## License

This project is licensed under the Apache 2 License

## Acknowledgments

* This work was primarily funded by IBM Research AI Horizons Network with the Rensselaer Institute for Data Exploration and Applications (IDEA)

# Insurance-Fraud-Prediction
Objective: To predict the risk of Auto Insurance Fraud

Data Description: The data obtained can be found in the insurance_claims.csv file and it contains 1000 rows and 44 columns. The data can be split into 3 sections, namely:

  1. Details about the Policy: policy_day, policy_month, policy_year, policy_state, policy_deductible, policy_annual_premium, and so on

  2. Details about the Insured: insured_name, insured_state, insured_occupation, insured_hobbies, and so on

  3. Details about the Claim: incident_type, collision_type, incident_severity, authorities_contacted, incident_state, and so on The target variable is the binary variable, fraud_reported

A Logistic Regression Model was built using the above data, to predict if a claim has a risk of being fraudulent or not. A RShiny app was built for the same.

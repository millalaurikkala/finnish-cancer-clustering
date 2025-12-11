In this folder, the supplementary files used to produce the results of the article 'On Cluster Structures of Finnish 
Cancer Incidence Data' (2025) are provided. The contents and structure of the folder are as 
follows:

(i) 'cancer_registry_incidence_data.xlsx' contains the incidence data that is then analyzed in the article. The data 
is retrieved 11 Aug 2025 from: https://cancerregistry.fi/statistics/cancer-statistics/; and by selecting cancer 
sites selected individually, values are categorized by: Age group (10y.) and Year (1953-2023), and values shown by: 
Rate per 100,000.

(ii) 'data_preprocessing.ipynb' reads the contents of 'cancer_registry_incidence_data.xlsx' and performs some 
prerocessing steps to the data. The preprocessed is stored into a CSV-file called 'incidence.csv' that is then later 
accessed by the file 'preliminary_analysis.R' described below.

(iii) 'incidence.csv' contains the data preprocessed by the file 'data_preprocessing.ipynb'; see 
above.

(iv) 'preliminary_analysis.R' reads the contents of 'incidence.csv' and is used to further process the data. Most 
importantly, the 10 most common cancer types are identified for each subgroup determined by age and gender. The 
corresponding, processed data, is stored to the CSV-called 'cancer_data_most_common_cancers.csv'. Additionally, this 
file is utilized to produce some supplementary figures.

(v) 'cancer_data_most_common_cancers.csv' contains the data processed by the file 'preliminary_analysis.R'; see 
above.

(vi) 'agg_hierarch_clustering_cancers.R' reads the contents of 'cancer_data_most_common_cancers.csv' and is used to 
performs the agglomerative hierarchical clustering for that data. The corresponding clustering results are presented
as figures.

(vii) the subfolder 'most_common_cancers_incidence_ts' contains the figures of time series representing the incidence rates 
per 100,000 person-years of the 10 most common cancers among females and males in different age groups from 1963 to 2023. The
code to produce these figures is included in the file 'preliminary_analysis.R'.
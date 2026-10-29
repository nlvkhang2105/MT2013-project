
# ============================================================
# T05 - Data Preprocessing
# Owner: Tue
# Goal: Prepare data for descriptive and inferential analysis
# ============================================================

source("MT2013/scripts/load_data.R")
df <- load_data()

# Selected variables:
# Width : X1    - Image width
# Class : X1558 - Ad / Non-ad
# F1    : X351  - url*ads
# F2    : X1455 - alt*click+here


# TODO 1: Select and validate the analysis variables.
# Check data types, valid values, and Class labels.


# TODO 2: Assess missing values and duplicate records.
# Create missing_by_class for Width missingness.
# Create duplicate_rows using all columns except Index.
# Do not remove duplicate records.


# TODO 3: Prepare and validate analysis datasets.
# analysis_df: Selected variables, retaining missing values.
# rq1_data: Complete Width and Class observations.
# rq2_data: Separate complete datasets for F1 and F2.


# Outputs: analysis_df, rq1_data, rq2_data
#          missing_by_class, duplicate_rows

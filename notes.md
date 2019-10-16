Notes
================

## Data distribution plans

### Regular/novice users

Novice versions of data can go on data.world in wide table formats.
These are similar to the formatted tables in Excel files that we gave to
the designers. data.world takes datasets which can be made up of
multiple files. The best way to do this might be to have datasets by
source or type, so one dataset of CWS data, then maybe one dataset for
each chapter or set of topics. DW can take Excel-type spreadsheets, so
each table’s data could actually be a sheet in a workbook, just like the
formatted tables.

#### Example structure

    DataHaven on data.world
    .
    |-- CWS 2018 (by group and town within region)
    |   |-- Greater New Haven CWS
    |   |-- Fairfield County CWS
    |   |-- Greater Hartford CWS
    |-- Demographics 2017 (by town and/or demographics within region)
    |   |-- Greater New Haven demographics (workbook)
    |   |   |-- Population & growth (table 2A) (sheet)
    |   |   |-- Race & immigration (table 2B) (sheet)
    |-- |-- Greater Hartford demographics (workbook)
    |   |   |-- Population & growth (table 2A) (sheet)
    |   |   |-- Race & immigration (table 2B) (sheet)

For survey data, novice version for Greater New Haven would look like:

| category       | group       | satisfied\_with\_area | local\_govt\_responsive | police\_approval | suitable\_employment |
| :------------- | :---------- | --------------------: | ----------------------: | ---------------: | -------------------: |
| Connecticut    | Connecticut |                  0.82 |                    0.51 |             0.78 |                 0.50 |
| Total          | Total       |                  0.82 |                    0.48 |             0.75 |                 0.49 |
| Gender         | Male        |                  0.82 |                    0.47 |             0.76 |                 0.49 |
| Gender         | Female      |                  0.82 |                    0.49 |             0.75 |                 0.49 |
| Age            | Ages 18-34  |                  0.83 |                    0.42 |             0.67 |                 0.49 |
| Age            | Ages 35-49  |                  0.82 |                    0.48 |             0.74 |                 0.50 |
| Age            | Ages 50-64  |                  0.82 |                    0.44 |             0.78 |                 0.46 |
| Age            | Ages 65+    |                  0.84 |                    0.60 |             0.85 |                 0.51 |
| Race/Ethnicity | White       |                  0.84 |                    0.52 |             0.83 |                 0.52 |
| Race/Ethnicity | Black       |                  0.78 |                    0.31 |             0.60 |                 0.39 |

Data dictionary gets written in a separate text file and uploaded as
part of dataset metadata, so that will help make sure all CWS data has
the same descriptions for each indicator.

### Advanced users

Advanced use data will be in long-shaped CSV files that are more
appropriate for doing data analysis. These are similar to the types of
files in the index output\_data folders. For now, these will just be on
github, although maybe some other storage, like a cloud database or S3
bucket would make sense as well. For ACS, etc. it’s probably easiest for
these to have a similar folder structure to the 2019index repo, e.g.:

    2019indexpub repo
    .
    |-- demographics
    |-- |-- population_by_age_2017.csv
    |-- |-- population_by_race_2017.csv
    |-- |-- poverty_and_low_income_by_age_2017.csv
    |-- |-- household_structure_2017.csv
    |-- housing
    |-- |-- median_housing_value_2017.csv
    |-- |-- homeownership_total_and_by_race_2017.csv
    |-- |-- housing_cost_burden_by_tenure_2017.csv

For CWS data, these will just be dumps of all survey data for a
location, plus its weights in a separate file, then an overall lookup
table.

Advanced version of CWS data for Greater New Haven looks like:

| code   | category | group  | response          | value |
| :----- | :------- | :----- | :---------------- | ----: |
| LIFECC | Gender   | Male   | Almost certain    |  0.02 |
| LIFECC | Gender   | Male   | Very likely       |  0.05 |
| LIFECC | Gender   | Male   | A toss up         |  0.14 |
| LIFECC | Gender   | Male   | Not very likely   |  0.35 |
| LIFECC | Gender   | Male   | Not at all likely |  0.39 |
| LIFECC | Gender   | Male   | Don’t know        |  0.04 |
| LIFECC | Gender   | Male   | Refused           |  0.00 |
| LIFECC | Gender   | Female | Almost certain    |  0.03 |
| LIFECC | Gender   | Female | Very likely       |  0.06 |
| LIFECC | Gender   | Female | A toss up         |  0.11 |
| LIFECC | Gender   | Female | Not very likely   |  0.38 |
| LIFECC | Gender   | Female | Not at all likely |  0.35 |
| LIFECC | Gender   | Female | Don’t know        |  0.06 |
| LIFECC | Gender   | Female | Refused           |  0.00 |

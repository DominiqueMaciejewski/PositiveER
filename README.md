# Positive ER in Daily Life

This is the GitHub Repo for the Analyses for the Paper "Positive Emotion Regulation in Daily Life: Associations with Momentary Emotions and Anhedonia".

The data uploaded here have been anonymized by replacing the ID number with another random ID number and rounding age to years without decimals.

If you have any questions, anything is unclear and/or you find a mistake, please do not hesitate to contact me under d.f.maciejewski\@tilburguniversity.edu.

## Main Directory

| Filename                           | Description                                       |
|------------------------------|--------------------------------------|
| `.gitignore`                       | Gitignore file                                    |
| `2023_7_18_Rcode_PositiveER.R`     | R code to reproduce all results                   |
| `2023_7_18_Rcode_PositiveER.html`  | Knitted version of the code                       |
| `Mplus Models.logRun`              | Mplus log file                                    |
| `PositiveER_Analyses.Rproj`        | RProject                                          |
| `README.md`                        | Readme for Github page                            |
| `correlation.xlxs`                 | Output of between- and within-person correlations |

## Folder: Data

In this folder, you can find the processed data. Because, we included sensitivity analyses with regards to the compliance inclusion criterion and the item combination, we generated multiple data sets and re-ran analyses across all those in a multiverse fashion.

In total, there are 15 different data files (all are provided in .csv, .dat and .RData format):

| Filename | Description                                                                                                                    |
|----------|--------------------------------------------------------|
| data0    | 33% compliance inclusion criteria (pre-registered) & NA and PA scales with all items -\> *Used for main analyses of the paper* |
| data1    | no inclusion criterion based on compliance & NA and PA scales with all items                                                   |
| data2    | 33% compliance inclusion criteria (pre-registered) & PA enthusiast (enthusiastic) item removed                                 |
| data3    | 33% compliance inclusion criteria (pre-registered) & PA tevreden (satisfied) item removed                                      |
| data4    | 33% compliance inclusion criteria (pre-registered) & PA energiek (energetic) item removed                                      |
| data5    | 33% compliance inclusion criteria (pre-registered) & PA kalm (calm) item removed                                               |
| data6    | 33% compliance inclusion criteria (pre-registered) & PA daadkrachtig (determined) item removed                                 |
| data7    | 33% compliance inclusion criteria (pre-registered) & PA vrolijk (cheerful) item removed                                        |
| data8    | 33% compliance inclusion criteria (pre-registered) & PA dankbaar (grateful) item removed                                       |
| data9    | 33% compliance inclusion criteria (pre-registered) & NA geirritieerd (irritated) item removed                                  |
| data10   | 33% compliance inclusion criteria (pre-registered) & NA Verveeld (bored) item removed                                          |
| data11   | 33% compliance inclusion criteria (pre-registered) & NA nerveus (nervous) item removed                                         |
| data12   | 33% compliance inclusion criteria (pre-registered) & NA verdrietig (sad) item removed                                          |
| data13   | 33% compliance inclusion criteria (pre-registered) & NA boss (angry) item removed                                              |
| data14   | 33% compliance inclusion criteria (pre-registered) & NA somber (low) item removed                                              |

## Folder: Scripts

**Note**: The overall `R` script with all analyses is in the main folder.

The file `2023_7_18_Rcode_PositiveER.R` contains the code.

The file `2023_7_18_Rcode_PositiveER.html` contains the knitted document including the results.

In the folder scripts, you can find the Mplus scripts (.inp files) plus the associated output (.out files).

The subfolders are structured according to our research questions.

Note that research question 1a was answered using descriptive statistics in R (please refer to for `2023_7_18_Rcode_PositiveER.html` results for 1a).

Research questions 1b-2b were answered using DSEM Models run in Mplus via R with the package `MplusAutomation`.

| Subfolder | Description research question                                                                                                    |
|-----------|-------------------------------------------------------------|
| RQ1b      | Within-person relation between emotions at t-1 and positive ER strategies at t (identification)                                  |
| RQ1c      | Within-person relation between positive ER strategies at t and emotions at t (implementation)                                    |
| RQ2a      | Main-effects of anhedonia on ER strategies at t (selection)                                                                      |
| RQ2b      | Moderation of anhedonia on within-person relation between emotions at t-1 and positive ER strategies at t (identification)       |
| RQ2c      | Moderation effects of anhedonia on within-person relation between positive ER strategies at t and emotions at t (implementation) |
| Templates | Mplus Templates used for looping over different data and item combinations. See `MplusAutomation` for more information.          |

In each folder, the data files are structured like this: `RQx-ERStrategy-dataset.inp/out`. For instance, `RQ1b-PER_att-data0.inp` is the input file for the model for hypothesis 1b for the strategy attention for dataset 0 (the main results of the paper).

## Folder: Tables

Contains the generated tables reported in the paper.
Note that the table numbering changed shortly before submission. Moreover, not all tables were generated in R. As such, the numbering is not exactly the same between the Github repo and the submitted paper.

Here is an overview:

| Title                               | Name in Github | Name in Paper |
|-------------------------------------|-----------------|----------------|
| Descriptive Statistics for the Positive ER Strategies| table_1.docx | Table 1 |
| Emotions at t-1 and the Use of Positive ER Strategies at t - Within-Person Effects and Moderation by Anhedonia  | table_3.docx | Table 2 |
| Use of Positive ER strategies at t and Emotions at t - Within-Person Effects and Moderation by Anhedonia | table_4.docx | Table 3 |
| Study Details of Diary and ESM Studies on Positive ER in Daily Life | Not created in R | Supplementary Table 1 |
| Sample Characteristics | Not created in R | Supplementary Table 2 |
| Within-person and between-person correlation of ER strategies  | table_2.docx | Supplementary Table 3 |
| FDR-corrected significance thresholds for 63 tested hypotheses  | Not created in R | Supplementary Table 4 |
| Emotions at t-1 and the Use of Positive ER Strategies at t - Within-Person Effects and Moderation by Anhedonia and Depression  | table_sup1.docx | Supplementary Table 5 |
| Use of Positive ER strategies at t and Emotions at t - Within-Person Effects and Moderation by Anhedonia | table_sup2.docx | Supplementary Table 6 |



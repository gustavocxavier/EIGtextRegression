
## How to Run the Code
The .R files in this folder requires a connection to WRDS servers. Below we
outline our preferred approach to creating the dataset:

1. Use [SSH to connect to the WRDS Cloud](https://wrds-www.wharton.upenn.edu/pages/support/the-wrds-cloud/using-ssh-connect-wrds-cloud/).
2. Send this file to your home directory.
3. Run the scrips in the following order:
    - getWRDSdata.R
    - getSECdata.R
    - getTerms.R
    
<!--
1. Connect to the [SAS studio server hosted by WRDS](https://wrds-cloud.wharton.upenn.edu/SASStudio/index?locale=en_US).  
2. Create a folder called _Global Data_ in your home directory and upload `main.sas`, `project_macros.sas`, `market_chars.sas`, `accounting_chars.sas`, `char_macros.sas`, `portfolios.sas`, `ind_identification.sas` and `chars.xlsx` to this folder.
3. Create an empty folder in your institutions scratch folder. The scratch folder is located at "Sever Files and Folders/Files/scratch/\<institution name\>".
4. Open `main.sas` 
5. Replace line 7 with the path to the scratch folder created in step 3. 
6. Run `main.sas`. 
-->

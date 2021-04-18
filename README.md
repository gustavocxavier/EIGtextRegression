Overview
--------

This repository contains code to create the dataset for my work in progress called `A Text-based Measurement of Expected Investment Growth`, which use text regression and machine learning to forecast the expected investment growth at the firm level. To reproduce, you can either clone this repository with git, or download the entire content as a zip file by clicking on the `Download ZIP` button on `Code` menu above. Than onpen `start.R` in your R Studio.

How to Run the Code
-------------------
Open `start.R` in your R studio. The code creates some subfolders where it will store temporary data for each step, so that you don't have to run the entire
code more than once. 

CORE PRINCIPLE:
--------------
1. Never modify 0_data
2. Only save/load to "pipeline" folder

Additional notes
----------------
- I structured this code based on [Ties de Tok tips](https://arc.eaa-online.org/blog/how-keep-your-projects-organized-part-1-folder-structure)

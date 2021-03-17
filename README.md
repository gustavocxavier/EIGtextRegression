Overview
--------

This repository contains code to create the dataset for my work in progress called A Text-based Measurement of Expected Investment Growth, which I use text regression and machine learning to forecast the expected investment growth at the firm level. You can either clone this repository with git, or download the entire content as a zip file by clicking on the "Download ZIP" button on "Code" menu above.

How to Run the Code
-------------------
Open start.R in your R studio

CORE PRINCIPLE:
--------------
1. Never modify 0_data
2. Only save to "pipeline" folder
3. Only load from "2_pipeline/2_out" folder or "0_data"
4. Anything in "2_pipeline/0_tmp" can be deleted

Anout 2_pipeline:
----------------
- 0_tmp contains files that I save for inspection purposes or some other temporary reason. The basic principle is that we should not have to worry about anything in the tmp folder being deleted as it only serves a temporary purpose.

- 1_store contains files that I save with the intention of loading them in the current code file. This is, for example, used in scenarios where it takes a while to run parts of your code and to avoid having to re-run these parts every time I might want to intermittently save the progress of your generated data to the store folder.

- 2_out contains files that I save with the intention of loading them in a future code file. These are usually the “end-products” of the current code file.

Additional notes
----------------
- I structured this code based on [Ties de Tok tips](https://arc.eaa-online.org/blog/how-keep-your-projects-organized-part-1-folder-structure)



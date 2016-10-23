# AV_Knocktober
Analytics Vidhya, Knocktober Competition
https://datahack.analyticsvidhya.com/contest/knocktober-2016

Solution for mark12 (Mark Landry)

*Models used*: H2O GBM (x4), H2O Random Forest (x1)
Blend that favored my strongest GBM, but also included a segmented GBM (based on camp type), a random forest, and a couple slight variations of the GBM hyperparameters.

Feature engineering is in data.table. There is a lot of code to ensure the target-based features are adjusted for the particular row being predicted. That's probably the hardest part to follow in the code.



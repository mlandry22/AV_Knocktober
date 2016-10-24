# AV_Knocktober
Analytics Vidhya, Knocktober Competition
https://datahack.analyticsvidhya.com/contest/knocktober-2016

Solution for mark12 (Mark Landry)

## Summary
*Models used*: H2O GBM (x4), H2O Random Forest (x1)  

Blend that favored my strongest GBM, but also included a segmented GBM (based on camp type), a random forest, and a couple slight variations of the GBM hyperparameters.

Feature engineering is in data.table. There is a lot of code to ensure the target-based features are adjusted for the particular row being predicted. That's probably the hardest part to follow in the code.

### Minor notes to help read the code
copied from outside discussions, maybe it helps :-)  

It's not particularly easy to follow, with few comments. But I was fairly happy with my features. Time-based holdout sets are hard to know what to do with the style of features I was working toward. Other than simple date differences, the main style of feature is calculating the target rate for various things. And with a time-based holdout, it meant that you hadn't seen many of the patients before. So there are still some features about that--whether or not they attended (usually I call those "N"). But all the rate calculations will be degraded over time for the newest people. So in my "ideas" section, you can see an unimplemented type of feature that goes after a fixed time period, or recency. That would standardize everything for everybody a little better. I don't know if it would have helped or not, and wish I had decided to try. But I didn't have enough time on the weekend to do much other than mindless tuning.  

Since it is not commented, all the occurrences of calculations with division and in the numerator and denominator there are ifelse statements, that is my chosen method to not leak the actual answer of the current camp. So in one pass I get the full training history, and then when predicting each of the instances, remove the effect of the current instance from all calculations. That part is fairly important, and I believe it's implemented correctly. It does mean instances can look into the future. But I was ok with that for this purpose.

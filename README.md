# FeatureEngineeringAndStatisticalAnalysis
This project was from my CSC3060 module (Artificial Intelligence and Data Analytics). It is the first assignment in a 2 part project. 

There were 3 parts to this project:

(1) Create a dataset of handwritten symbols.
(2) Calculate features from the handwritten symbols which may be useful for distinguishing between the different symbols automatically.
(3) Perform statistical analysis of the datasets, using methods of statistical inference. 

We created the handwritten symbols by using a tool called GIMP, where we used the touchscreen software of the PCs in the computer science lab to draw our own handwritten symbols. Each separate symbol had 8 different attempts so as to have variance within each symbol group. We then had to export these symbols to a pgm file which were binary where 1 is black and 0 is whitespace. These pgm files were converted to csv files via the notebook in the directory section1_code. 

After this, our handwritten symbols were now more easily processable. We used this to our advantage by calculating a range of features for each handwritten symbol. This is done in the notebook in section2_code. In this notebook we calculate a range of features, such as how many neighbours are around each pixel or if the symbol has an 'eye' (for example letters like a and e have eyes but c and f do not and we assign calculated features to each training sample which gives each sample a feature vector.

These features were then statistically analysed and visualised in section 3 using R code. Comparison between the symbol groups was visualised using boxplots and also calculated using the ANOVA statistical method as well as t-tests for when the comparison was between two groups rather than multiple. The Tukey HSD method was used especially often as a statistical method to compare across multiple features for different groups of symbols.





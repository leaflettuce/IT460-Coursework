# Final Project

Final Project for SNHU Machine Learning Class.


### Project Description
```
Classifying images based upon image size and keyword terminology to determine if image is an ad or not.
Dataset obtained from UCI data repository. Contains 1559 variables and ~3200 observations. 
Around 15% of data are classified as ads.
```

### Process
```
Data preprocessed in process_data.R
ANN, knn, and nb files fit neural net, nearest neighbor, and naive bayes models, respectively.
Caret confusion matrix used to evaluate results for accuracy, kappa, sensitivity, and specificity.
```


### Results
```
All models fit relatively accurate at >95% accuracy each.
Kappa scores fluctuate slightly more. 
Fine-tuned ANN w/ 1 hidden neuron modeling the 'best'. 

ANN: acc- 97%, kappa- 88%
kNN: acc- 95%, kappa- 77%
 nb: acc - 96%, kappa- 86%
```

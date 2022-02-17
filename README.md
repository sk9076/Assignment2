# Assignment 2

Sooyoung Kim (sk9076@nyu.edu)


### Package requirement
<b><font color = red>You need to have 'pacman' package for the apps to run successfully (pacman::p_load() install/loads all the required packages)</b></font>

```
install.packages('pacman')
```

### Version 1

<p>This application compares 2 separate linear regression fit of the models derived from the same dataset.
         To run this app without any issue, you will need; </p>
<ul>
  <li>a dataset in <u><b>.csv </u></b>format; and</li>
  <li><u><b>at least ONE</u></b> continuous variable in the dataset to be used as outcome variable</li>
</ul>

<p> Two models will share a <b> common outcome variable</b> of your choice. The model fit comparison will be
conducted using the the <b>Alkaike Information Criterion (AIC)</b>. If two models are nested (meaning the predictor(s) of one
model is a subset of the other model), <b>the F-statistics</b> with p-value will also be computed.</p>

<p>You can click the <b> Start </b> button to begin.</p>

<br>
         
         
### Version 2

<p>This application lets you fit linear regression models using the dataset of your choice. To run this app without any issue, you will need; </p>
<ul>
 <li>a dataset in <u><b>.csv </u></b>format; and</li>
 <li><u><b>at least ONE</u></b> continuous variable in the dataset to be used as outcome variable</li>

</ul>
<p>Once you fit the model, you will be able to save the<b> summary of the results</b> and model fit statistics, represented by the <b>Alkaike Information Criterion (AIC)</b>. You can call the saved results from the drop-down menu whenever you want.</p>

<p>Additionally, you can choose models from the saved list to compare their leave-one-out cross-validation errors. </p>
<p>At any step, if you'd like to re-start by uploading a new data set, click the <b>Reset and start again</b> button on the upper-right corner.</p>
 
<P> You can click the <b> Start </b> button to begin.<br><br>



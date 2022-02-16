# Assignment 2

Sooyoung Kim (sk9076@nyu.edu)


### Version 1

<p>This application compares 2 separate linear regression fit of the models derived from the same dataset.
         To run this app without any issue, you will need; </p>
<ul>
  <li>a dataset in <u><b>.csv </u></b>format; and</li>
  <li><u><b>at least ONE</u></b> continuous variable in the dataset to be used as outcome variable</li>
</ul>

<p> Two models will share a <b> common outcome variable</b> of your choice. The model fit comparison will be
conducted using the <b>the F-statistics</b> if two models are <u>nested</u> (meaning the predictor(s) of one
model is a subset of the other model), or the <b>Alkaike Information Criterion (AIC)</b>
if two models are <u>NOT</u> nested. </p>

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

### Pending issues I didn't get to resolve

<p>There was one issue I couldn’t resolve and it was about triggering the fields to reset by “reset” button.
I attached is the simplified reproducible example in the folder "reproducible_ex" for your information. Basically, when I click on the “reset” button, I can see that the values in rv$vars gets updated to NULL, but this does not trigger the update on selectInput, which is coded in lines 49-52.

Is it a normal Shiny behavior that observeEvent does not respond to reactive value changes triggered within the server.R, not by the user input? What would be the alternative coding approach to make this happen?</p>

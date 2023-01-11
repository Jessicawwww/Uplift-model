# Uplift-model

## Why uplift model
To calculate the increase amount brought by certain actions, since in real life, we cannot calculate the incrementality for one single user. 
The models that can be used here:
1. meta-learner
T-learner



This project is AU C2C seller campaign targeting Strategy Optimization. 
Our platform will send coupons to sellers to encourage them to add more listings. 
Our challenge here is to prevent revenue loss for FVF(final value fee discount) cap. So our main purpose here is to only target those persuadable customers(with highest persuadable likelihood score).  
![uplift1](uplift_model.png)

To simply summarize the differences between s-learner, t-learner and x-learner:
- T-learner: 
build models for both treatment and control groups and calculate the difference. Calculate the listing probability of samples in test set when given a treatment based on model from train set in treatment group and also calculate probability when not given a treatment based on model from train set in control group. This model is simple and direct and can use any single model, for example, LR, SVM, NN. 
But this model will increase the variances of two models and when multiple treatments exist, we need to train several models. 
- S-learner: 
use whether we apply treatment or not to create a new label  

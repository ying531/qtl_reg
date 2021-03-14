
# Example

####################################################
##### one-sided cqr ##### f(x) = hat{q}(x,beta) + eta* for some eta*
# - suppose you have a dataframe `data`
# - covariate_name is the vector of colnames of X-covariates
# - response_name is the colname of response

beta = 0.5 # target of quantile

# split data
split_data = Split.Data(data, 1/3, 1/3) #equal-sized folds
train.data = split_data$train
calib.data = split_data$calib
test.data = split_data$test

### train the quantile regression model
# use ctree
mdl.ctree = qtl_train(train.data, covariate_name, response_name, method='ctree')
# use quantile regression forest
mdl.qrf = qtl_train(train.data, covariate_name, response_name, method='qrf', other.args= list("num.threads" = 1))

### compute the fitted quantiles
cal.q.ctree = qtl_pred(mdl.ctree, calib.data[covariate_name], beta, method='ctree')
cal.q.qrf = qtl_pred(mdl.qrf, calib.data[covariate_name], beta, method='qrf')

### compute scores on calibration set
cal.s.ctree = calib.data[,response_name] - cal.q.ctree
cal.s.qrf = calib.data[,response_name] - cal.q.qrf

### cqr calibrate
cqr.eta.ctree = as.numeric(quantile(cal.s.ctree, beta*(1+1/dim(calib.data)[1])))
cqr.eta.qrf = as.numeric(quantile(cal.s.qrf, beta*(1+1/dim(calib.data)[1])))

### calibrated prediction
test.q.ctree = qtl_pred(mdl.ctree, test.data[covariate_name], beta, method='ctree')
test.q.qrf = qtl_pred(mdl.qrf, test.data[covariate_name], beta, method='qrf')
hatq.ctree = test.q.ctree + cqr.eta.ctree
hatq.qrf = test.q.qrf + cqr.eta.qrf

####################################################
##### one-sided distributional conformal ##### f(x) = hat{q}(x, eta*) for some eta*
# - suppose you have a dataframe `data`
# - covariate_name is the vector of colnames of X-covariates
# - response_name is the colname of response

beta = 0.5 # target of quantile

# split data
split_data = Split.Data(data, 1/3, 1/3) #equal-sized folds
train.data = split_data$train
calib.data = split_data$calib
test.data = split_data$test

### train the quantile regression model
# use ctree
mdl.ctree = qtl_train(train.data, covariate_name, response_name, method='ctree')
# use quantile regression forest
mdl.qrf = qtl_train(train.data, covariate_name, response_name, method='qrf', other.args= list("num.threads" = 1))

### compute scores (the fitted cdf) on calibration set
X.cal = calib.data[covariate_name]
Y.cal = calib.data[,repsonse_name]
hatp.cal.ctree = cdf_pred(mdl.ctree, X.cal, Y.cal, method='ctree', eps=0.001)
hatp.cal.qrf = cdf_pred(mdl.qrf, X.cal, Y.cal, method='qrf', eps=0.001)

### calibrate fitted cdf
p.ctree = as.numeric(quantile(hatp.cal.ctree, beta*(1+1/dim(calib.data)[1])))
p.qrf = as.numeric(quantile(hatp.cal.qrf, beta*(1+1/dim(calib.data)[1])))

### calibrated prediction
hatq.ctree = qtl_pred(mdl.ctree, test.data[covariate_name], p.ctree, method='ctree')
hatq.qrf = qtl_pred(mdl.ctree, test.data[covariate_name], p.qrf, method='ctree')




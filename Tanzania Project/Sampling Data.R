##########################sampling the training data########################

## For SVM - Optional (to test for downsampling)
## Run the code below after running preprocessing
## Run the SVM model to test different downsampling

############################################################################


summary(data_train$status_group)

func = data_train_pre[data_train_pre$status_group == 'functional',]
non_func = data_train_pre[data_train_pre$status_group == 'non functional',]
func_needs_repair = data_train_pre[data_train_pre$status_group == 'functional needs repair',]
str(data_train_pre)

table(data_train_pre$status_group)

## Modify here to change the sampling size
dwnsample = 0.5

##
class_size = (1-dwnsample)/2

dim(func_needs_repair)[1]

needs_repair_id = sample(dim(func_needs_repair)[1],2158)
func_needs_repair_valid = func_needs_repair[-needs_repair_id,]
func_needs_repair = func_needs_repair[needs_repair_id,]

func_id = sample(dim(func)[1],class_size*(2158/dwnsample))
func_valid = func[-func_id,]
func = func[func_id,]

non_func_id = sample(dim(non_func)[1],class_size*(2158/dwnsample))
non_func_valid = non_func[-non_func_id,]
non_func = non_func[non_func_id,]

data_train_sampled = rbind(func,non_func)
data_train_sampled = rbind(data_train_sampled,func_needs_repair)

data_train_valid = rbind(func_valid,non_func_valid)
data_train_valid = rbind(data_train_valid,func_needs_repair_valid)

dim(data_train_valid)

#########################

set.seed(1)
x = data_train_sampled
xt = data_train_valid

#########################

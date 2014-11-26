import pandas as pd
from sklearn import ensemble
import numpy as np

def LoadData(loc):
	print("Load data")
	df = pd.read_csv(loc)
	return df;

def FeatureEngineering(df):
	print("Feature engineering")
	df['dist2water'] = pd.Series(df['Horizontal_Distance_To_Hydrology']*df['Horizontal_Distance_To_Hydrology']
	+df['Vertical_Distance_To_Hydrology']*df['Vertical_Distance_To_Hydrology'], index=df.index)
	df['d11'] = pd.Series(abs(df['Horizontal_Distance_To_Hydrology']+df['Horizontal_Distance_To_Roadways']), index=df.index)
	df['d12'] = pd.Series(abs(df['Horizontal_Distance_To_Hydrology']-df['Horizontal_Distance_To_Roadways']), index=df.index)
	df['d21'] = pd.Series(abs(df['Horizontal_Distance_To_Hydrology']+df['Horizontal_Distance_To_Fire_Points']), index=df.index)
	df['d22'] = pd.Series(abs(df['Horizontal_Distance_To_Hydrology']-df['Horizontal_Distance_To_Fire_Points']), index=df.index)
	df['d31'] = pd.Series(abs(df['Horizontal_Distance_To_Roadways']+df['Horizontal_Distance_To_Fire_Points']), index=df.index)
	df['d32'] = pd.Series(abs(df['Horizontal_Distance_To_Roadways']-df['Horizontal_Distance_To_Fire_Points']), index=df.index)
	df['EVDtH'] = df.Elevation-df.Vertical_Distance_To_Hydrology
	df['EHDtH'] = df.Elevation-df.Horizontal_Distance_To_Hydrology*0.2
	df['ell1'] = pd.Series(df['Hillshade_3pm']*df['Hillshade_3pm']+df['Hillshade_9am']*df['Hillshade_9am'], index=df.index)
	df['ell2'] = pd.Series(df['Hillshade_3pm']*df['Hillshade_3pm']+df['Hillshade_Noon']*df['Hillshade_Noon'], index=df.index)
	df['ell3'] = pd.Series(df['Hillshade_Noon']*df['Hillshade_Noon']+df['Hillshade_9am']*df['Hillshade_9am'], index=df.index)
	df['shaderatio1'] = pd.Series(df['Hillshade_9am']/(df['Hillshade_Noon']+1), index=df.index)
	df['shaderatio2'] = pd.Series(df['Hillshade_3pm']/(df['Hillshade_Noon']+1), index=df.index)
	df['shaderatio3'] = pd.Series(df['Hillshade_3pm']/(df['Hillshade_9am']+1.0), index=df.index)
	df['meanShade'] = pd.Series(df['Hillshade_9am']+df['Hillshade_Noon']+df['Hillshade_3pm'], index=df.index)
	df['Highwater'] = df.Vertical_Distance_To_Hydrology < 0
	df['VertDist'] = df.Vertical_Distance_To_Hydrology.abs();
	df['ratiodist1'] = pd.Series(abs(df['Horizontal_Distance_To_Hydrology']/(abs(df['Vertical_Distance_To_Hydrology'])+1)), index=df.index)
	df['ratiodist2'] = pd.Series(abs(df['Horizontal_Distance_To_Hydrology']/(df['Horizontal_Distance_To_Fire_Points']+1)), index=df.index)
	df['ratiodist3'] = pd.Series(abs(df['Horizontal_Distance_To_Hydrology']/(df['Horizontal_Distance_To_Roadways']+1)), index=df.index)
	df['ratiodist4'] = pd.Series(abs(df['Horizontal_Distance_To_Fire_Points']/(df['Horizontal_Distance_To_Roadways']+1)), index=df.index)
	df['maxdist'] = pd.Series(df[['Horizontal_Distance_To_Hydrology','Horizontal_Distance_To_Fire_Points',\
	'Horizontal_Distance_To_Roadways']].max(axis=1), index=df.index)
	
	return;


def TrainForest(df_train, ntree):
	feature_cols = [col for col in df_train.columns if col not in ['Cover_Type','Id']]
	X_train = df_train[feature_cols]
	y = df_train['Cover_Type']
	test_ids = df_test['Id']
	print("Training...")
	clf = ensemble.ExtraTreesClassifier(n_estimators = ntree, n_jobs = -1,verbose=1)
	clf.fit(X_train, y)
	return clf;

def DefTestSet(df_test):
	feature_cols = [col for col in df_test.columns if col not in ['Cover_Type','Id']]
	test_ids = df_test['Id']
	X_test = df_test[feature_cols]
	return test_ids, X_test;

def ExportData(loc_submission,clf,X_test,test_ids):
	print("Predicting")
	y_test = clf.predict(X_test)
	print("Writing data...")
	with open(loc_submission, "wb") as outfile:
		outfile.write("Id,Cover_Type\n")
		for e, val in enumerate(list(y_test)):
			outfile.write("%s,%s\n"%(test_ids[e],val))
	return y_test;


################
##### MAIN #####
################

loc_train = "train.csv"
loc_test = "test.csv"
loc_submission = "kaggle.forest.submission_features_new.csv"

# Loading data
df_train = LoadData(loc_train);
df_test = LoadData(loc_test);

# Features
FeatureEngineering(df_train);
FeatureEngineering(df_test);

rf = TrainForest(df_train,2000);

test_ids, X_test = DefTestSet(df_test);
#y_test = rf.predict(X_test);
#df_test['Cover_Type'] = pd.Series(y_test,index=df_test.index);

#df_combi = df_train.append(df_test);
#rf2 = TrainForest(df_combi,5);

ExportData(loc_submission,rf,X_test,test_ids);
pd.DataFrame(rf.feature_importances_,index=X_test.columns).sort([0], ascending=False) [:10]





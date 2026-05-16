#Define a method that indicates if a column contains NaN values.
def column_contains_na(df, df_column_name):
    b = df[df_column_name]
    contains_nan = b.isnull().values.any()
    return contains_nan

#Define a method that returns the most frequent value of a column given by argument.
def column_most_frequent_value(df , df_colum_name):
    column = df[df_colum_name]
    most_frequent_value = column.mode().values
    most_frequent_value = most_frequent_value[0,]
    return most_frequent_value

#Define a method that for a column of type date, replace NaN by an ancient date.
def column_na_date_replace_by_ancient_date(df, df_colum_name, replacement_value):
    
    df_updated = df
    if(isinstance(df_updated, pd.Series)):
        df_updated = pd.DataFrame(df_updated)
        df_updated.columns = [df_colum_name]
    b = df_updated[df_colum_name]
    b_updated = b.fillna(value=replacement_value)
    b_updated = pd.DataFrame(b_updated)
    df_updated = df_updated.drop(df_colum_name,axis=1)
    df_updated = pd.concat([df_updated,b_updated],axis=1)
    
    return df_updated

#Define a method that for a column of type object, replace NaN by the most frequent column value.
def column_na_object_replace_by_frequent(df, df_colum_name):
    df_updated = df
    replacement_value = column_most_frequent_value(df_updated,df_colum_name)
    if(isinstance(df_updated, pd.Series)):
        df_updated = pd.DataFrame(df_updated)
        df_updated.columns = [df_colum_name]
        
    b = df_updated[df_colum_name]
    b_updated = b.fillna(value=replacement_value)
    b_updated = pd.DataFrame(b_updated)
    df_updated = df_updated.drop(df_colum_name,axis=1)
    df_updated = pd.concat([df_updated,b_updated],axis=1)
    
    return df_updated

#Define a method that for a column of type object, replace NaN by the most frequent column value.
def column_na_float_replace_by_zeros(df, df_colum_name):
    df_updated = df
    if(isinstance(df_updated, pd.Series)):
        df_updated = pd.DataFrame(df_updated)
        df_updated.columns = [df_colum_name]
        
    b = df_updated[df_colum_name]
    b_updated = b.fillna(0)
    b_updated = pd.DataFrame(b_updated)
    df_updated = df_updated.drop(df_colum_name,axis=1)
    df_updated = pd.concat([df_updated,b_updated],axis=1)
    
    return df_updated

#Define a methods that returns true if a column is of type object.
def column_is_object(df , df_column_name):
    column = df[df_column_name]
    if (column.dtype =="object"):
        return True
    else:
        return False
        
#Define a method that combines all of the adjustments in order to modify the X_test.
def adjust_X_test(df):
    
    df_analysis = df
    df_columns = df.columns.tolist()
    
    for i in df_columns:
        
        #the column contains nan values?
        if(column_contains_na(df_analysis,i)==True):
            
            #The column is object?
            if(column_is_object(df_analysis,i)==True):
                #The column contains dates as strings?
                if (i=='hearing_date')or(i=='ticket_issued_date'):
                    df_analysis = column_na_date_replace_by_ancient_date(df_analysis,i,'2020-01-19 09:00:00')
                else:
                    df_analysis = column_na_object_replace_by_frequent(df_analysis, i)
            else:
                df_analysis = column_na_float_replace_by_zeros(df_analysis,i)
           
    return df_analysis

#Delete and update the X in cases where ticket_issued_dates are nan.
def delete_rows_ticket_issued_date_is_na(df):
    ticket_issued_date = df['ticket_issued_date']
    ticket_issued_date = ticket_issued_date.dropna()
    index_ticket_issued_date = ticket_issued_date.index.tolist()
    ticket_issued_date = ticket_issued_date.reset_index(drop=True)
    df_updated = df[df.index.isin(index_ticket_issued_date)]
    df_updated = df_updated.reset_index(drop=True)
    return df_updated, ticket_issued_date, index_ticket_issued_date

#Delete rows in a df containing NaN values for the column hearing_date.
#Returns the updated df, and the hearing df column for further analysis.
def delete_rows_hearing_date_is_na(df):
    hearing_date = df['hearing_date']
    hearing_date = hearing_date.dropna()
    index_hearing_date = hearing_date.index.tolist()
    hearing_date = hearing_date.reset_index(drop=True)
    df_updated = df[df.index.isin(index_hearing_date)]
    df_updated = df_updated.reset_index(drop=True)
    return df_updated, hearing_date, index_hearing_date

#Defining a method that calculates the number of months between the ticket issued date and the hearing date features.
#Add the column diff as the difference in montsh between colummns ticket issued date and hearing date..
#returns the df with the new column.
def diff_months_tiket_issued_and_hearing_date(df):
    ticket_issued_date_times = []
    hearing_date_times = []
    n_months_diff = []
    
    for i in range(0,df.shape[0]):
        a = str(df['ticket_issued_date'].values[i])
        a = datetime.strptime(a, '%Y-%m-%d %H:%M:%S')
    
        b = str(df['hearing_date'].values[i])
        b = datetime.strptime(b, '%Y-%m-%d %H:%M:%S')
    
        r = relativedelta.relativedelta(b, a)
        n_months = r.months
        n_months_diff.append(n_months)
        
    n_months_diff = pd.DataFrame(n_months_diff)
    n_months_diff.columns = ['n_months_diff_issued_hearing']

    df_updated = pd.concat([df,n_months_diff], axis =1)
    
    return(df_updated)

#Define a method that deletes rows in which the column hearing contains dates happened later than ticket issued dates.
#Deletes rows in which tikets_issued_date is later than hearing_date.
#Returns the df modified without unlogic rows.
def df_correct_issued_date_later_than_hearing_date(df ,is_test_set=False):
    if (is_test_set==False):
        correct_rows = df['n_months_diff_issued_hearing']
        correct_rows = (correct_rows>=0)
        correct_rows = df[correct_rows]
        index_correct_rows = correct_rows.index.tolist()
        df_updated = df[df.index.isin(index_correct_rows)]
        df_updated = df_updated.reset_index(drop=True)
        return df_updated, index_correct_rows
    else:
        correct_rows = df['n_months_diff_issued_hearing']
        correct_rows = pd.DataFrame(correct_rows)
        correct_rows.columns = ["n_months_diff_issued_hearing"]
        n_months_corrections = []

        for index, row in correct_rows.iterrows():
            months = row["n_months_diff_issued_hearing"]
            if(months<0):
                months = 30
            n_months_corrections.append(months)
    
        n_months_corrections = pd.DataFrame(n_months_corrections)  
        n_months_corrections.columns = ["n_months_diff_issued_hearing"]
        
        #Update the dataframe and its column n_months_diff_issued_hearing.
        df_updated = df
        df_updated = df_updated.drop('n_months_diff_issued_hearing',axis=1)
        df_updated = pd.concat([df_updated,n_months_corrections],axis =1)
        
        return df_updated
    

#Delete rows having NaN values in the column state for a dataframe passed by parameter.
#Returns the updated df, and the state column for further analysis.
def delete_rows_state_is_na(df):
    state = df['state']
    state = state.dropna()
    index_state = state.index.tolist()
    state = state.reset_index(drop=True)
    df_updated = df[df.index.isin(index_state)]
    df_updated = df_updated.reset_index(drop=True)
    return df_updated, state, index_state

#Delete rows in a df containing NaN values for the column violation_code.
#Returns the updated df, and the hearing df column for further analysis.
def delete_rows_violation_code_is_na(df):
    violation_code = df['violation_code']
    violation_code = violation_code.dropna()
    index_violation_code = violation_code.index.tolist()
    violation_code = violation_code.reset_index(drop=True)
    df_updated = df[df.index.isin(index_violation_code)]
    df_updated = df_updated.reset_index(drop=True)
    return df_updated, violation_code, index_violation_code

#Check features containing nan values.
def column_contains_nan(column):
    contains = False
    contains_all = False
    nans = column.isnull().sum()
    if nans > 0:
        contains = True
        if nans == column.shape[0]:
            contains_all = True
    return contains , contains_all

#Droping variables (columns) in df containing only nan values (all rows are nan).
def drop_nan_columns(df):
    df_modified = df
    for i in df_modified.columns:
        contains, contains_all = column_contains_nan(df_modified[i])
        if contains_all==True:
            df_modified = df_modified.drop([i], axis=1)    
    return df_modified

#Define a method that drops from the set of features the variables that aren't relevant in the analysis. 
#A list with the name of irrelevant variables is passed by argument.
def drop_not_relevant_column(df,list_columns):
    df_modified = df
    for i in list_columns:
        df_modified = df_modified.drop([i], axis=1)   
    return df_modified

#Define a method that creates a contingency table with the values in a column that maximixes the compliance rate..
#A column name should be passed by argument.
#The contingency contains the n_top values with more payed tickets in the columns passed by parameter..
#Gives the columns values having the best compliance ratio.
#Returns a df with the ordered values, and a list with their name.
def find_best_ratio_of_payment_column(df_column, df_target, df_column_name, df_target_name, n_top):
  
    df = pd.concat([df_column, df_target], axis =1)
    
    #Create contingency table df.
    df_contingency = pd.crosstab(df[df_column_name], df[df_target_name])
    contingency = df_contingency.values
    
    #Calculate the ratio of best payed tickets regarding on the feature values.
    ratio_best_payed = contingency[:,1]/np.sum(contingency,axis=1)
    ratio_best_payed = pd.DataFrame(ratio_best_payed)
    ratio_best_payed.index = df_contingency.index
    ratio_best_payed.columns = ['ratio_best_payed']
    ratio_best_payed = ratio_best_payed.sort_values(by=['ratio_best_payed'], ascending=False)
    
    #Select the n_top of the contingency ratio df.
    ratio_best_payed = ratio_best_payed.iloc[0:n_top,:]
    list_names = ratio_best_payed.index.tolist()
    
    #Return df_ratio from the contigency with the ordered values.
    return(ratio_best_payed ,list_names)

#Define a method that creates a df having the contingency values for the n_top most frequent values.
#Returns a df with the n_top values having more ticktes
def most_frequent_values_in_feature(df_column, df_y,  n_top):
    
    df_contingency_1 = pd.crosstab(df_column, y)
    df_contingency = df_contingency_1.values
    n_tickets = np.sum(df_contingency,axis=1)
    n_tickets = pd.DataFrame(n_tickets)
    n_tickets.index = df_contingency_1.index
    n_tickets.columns = ['n_tickets']
    n_tickets = n_tickets.sort_values(by=['n_tickets'], ascending=False)
    n_tickets = n_tickets.iloc[0:n_top,:]
    list_names = n_tickets.index.tolist()
    
    #Return df_ratio from the contigency with the ordered values.
    return(n_tickets ,list_names)

#Define a method that encodes dataframe categorical variables.
#A list of columns to encode, and the name of the column must be passed by argument.
#Returns the updated df with the encodings.
def encode_dummy_given_list(df, list_names, column_name):    
    df_update = df
    for i in list_names:
        column = (df_update[column_name]==i).astype(int)
        column = pd.DataFrame(column)
        column.columns = [column_name+"_"+i]
        df_update = pd.concat([df_update,column], axis =1)
        
    df_update = df_update.drop(column_name, axis=1)
    
    return df_update

#Droping variables (columns) having too many categories.
def drop_too_many_categories_in_column(df):
    df_modified = df
    for i in df_modified.columns:
        if(i != 'ticket_id'):
            if(df_modified.dtypes[i]=='object'):
                n_classes = df_modified[i].nunique()
                if n_classes >10:
                    df_modified = df_modified.drop([i], axis=1)   
    return df_modified


#Extract from the hearing date column the status and month for further analysis.
#Two additional features are created.
def hearing_date_feature_extraction(df_hearing_date):
    
    #tranform the type of the column hearing_date into date. 
    hearing_date = pd.to_datetime(df_hearing_date)
    
    #Extract the years column from hearing_date.
    hearing_date_years = hearing_date.dt.year
    hearing_date_years = pd.DataFrame(hearing_date_years)
    hearing_date_years.rename(columns = {'hearing_date':'hearing_date_year'}, inplace = True)
    hearing_date_years = hearing_date_years.astype('float64')
    
    #Extract the months column from hearing_date.
    hearing_date_months = hearing_date.dt.month
    hearing_date_months = pd.DataFrame(hearing_date_months)
    hearing_date_months.rename(columns = {'hearing_date':'hearing_date_month'}, inplace = True)
    hearing_date_months = hearing_date_months.astype('float64')
    
    #Extract the column hearing_date_status from the df.
    conditions = [
        (hearing_date_years['hearing_date_year'] <= 2011),
        (hearing_date_years['hearing_date_year'] > 2011) & (hearing_date_years['hearing_date_year'] <= 2015),
        (hearing_date_years['hearing_date_year'] > 2015)
    ]

    # create a list of the values we want to assign for each condition
    values = ['old', 'partially-old', 'recent']
    # create a new column and use np.select to assign values to it using our lists as arguments
    hearing_date_status = pd.DataFrame(np.select(conditions, values))
    hearing_date_status.rename(columns = {0: 'hearing_date_status'}, inplace = True)
    
    #Reset the indexes of the created dataframes.
    hearing_date_status = hearing_date_status.reset_index(drop=True)
    hearing_date_months = hearing_date_months.reset_index(drop=True)
    
    #Change the types of the hearing_date_months column.
    hearing_date_months = pd.DataFrame(hearing_date_months)
    hearing_date_months = hearing_date_months.astype(str)
    
    #return the extracted columns.
    return hearing_date_status, hearing_date_months

#Define a method that performs feature selection on the set of features.
#Select the n_top feauture variables from the dataset X which better explain y regarding on information gain.
#Information gain is based on entropy minimization when selection features.
#Returns a list with the names of the selected n_features.
def feature_selection_information_gain(X, y, n_top_features):
    mutual_info = mutual_info_classif(X,y)
    mutual_info = pd.DataFrame(mutual_info)
    mutual_info.index = X.columns
    mutual_info.columns = ['values']
    mutual_info.sort_values(by='values', ascending=False, inplace =True)
    
    df_relevant_features = mutual_info
    df_relevant_features = df_relevant_features.iloc[0:n_top_features,:]
    selected_features = df_relevant_features.index.tolist()
    print(df_relevant_features)
    return(selected_features)

#Select the best K features.
def feature_selection_information_gain_KBest(X, y, n_top):
    sel_cols = SelectKBest(mutual_info_classif, k = n_top)
    sel_cols.fit(X,y)
    selected_features = X.columns[sel_cols.get_support()].tolist()
    return(selected_features)

#Define a method that returns a series object with the ids and probabilities of compliance.
def get_probs_format(X, y_prob):
    ticket_predictions = pd.DataFrame(y_prob)
    ticket_predictions.index = X["ticket_id"]
    ticket_predictions = ticket_predictions.iloc[:,1:2]
    ticket_predictions.columns = ["compliance"]
    ticket_predictions = ticket_predictions.squeeze()
    return ticket_predictions



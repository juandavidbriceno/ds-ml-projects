#Check the missing values in columns within a pandas dataframe.
def cal_nan_percents(df):
    percent_missing = df.isnull().sum() / len(df)
    missing_value_df = pd.DataFrame({'column_name': df.columns,'percent_missing': percent_missing})
    percent_missing = percent_missing.to_frame()
    percent_missing.columns = ["Nan_percent"]
    percent_missing =  percent_missing.reset_index(drop=True)
    columns = pd.DataFrame(df.columns,columns=['columns_names'])
    df_nan_percents = pd.concat([columns,percent_missing],axis=1)
    return df_nan_percents


#Defining methods to fill up missing values within 100g columns in the dataframe.
def obtain_g_columns(df):
    df_mod = df
    g_columns = []
    for i in df_mod.columns:
        if (i[-5:] == '_100g'):
            g_columns.append(i)        
    return g_columns

#Defines a function that creates a mapping which fills nan values..
def create_column_mapping(df, col1, col2):
    mp = df.groupby([col1])[col2].mean()
    mp = mp.fillna(0).to_frame()
    mp.index.name = None
    mp[col1] = mp.index
    mp.reset_index(drop=True, inplace=True)
    mp = mp[[col1,col2]]
    mp = dict(mp[[col1, col2]].values)
    return mp

#Defines a function that fills the nan values.
def df_column_fill_missing_values(df, mp, col1, col2):
    df_to_mod = df
    df_to_mod[col2+'_'+'filling'] = df_to_mod[col1].map(mp)
    df_to_mod[col2] = df_to_mod[col2].fillna(df_to_mod[col2+'_'+'filling'])
    df_to_mod = df_to_mod.drop([col2+'_'+'filling'], axis=1)
    return df_to_mod

##Defines a function that filsls all the missing values dataframe's columns.
def df_fill_missing_values(df, col_of_avg):
    df_to_mod = df
    cols = obtain_g_columns(df)
    #cols.remove(col_of_avg)
    for i in cols:
        df_to_mod = df_column_fill_missing_values(df_to_mod,create_column_mapping(df_to_mod,col_of_avg,i),col_of_avg,i)
    
    return df_to_mod

#Define a method that obtains the indexes of nan rows within a df column.
def obtain_indexes_of_nan_values_in_column(df_column):
    rows_with_nan = []
    for index, row in df_column.to_frame().iterrows():
        is_nan_series = row.isnull()
        if is_nan_series.any():
            rows_with_nan.append(index)
    return rows_with_nan


#Defining methods to fil missing values in additives_n.
def obtaining_X_y_sets_for_additives_inputting(df):
    columns_features = obtain_g_columns(df)
    X_data_Knn = df[columns_features]
    y_data_Knn = df['additives_n']
    idx_nan = obtain_indexes_of_nan_values_in_column(y_data_Knn)
    X_data_Knn = X_data_Knn[~X_data_Knn.index.isin(idx_nan)]
    y_data_Knn = y_data_Knn[~y_data_Knn.index.isin(idx_nan)]
    return X_data_Knn, y_data_Knn

#Traing a Knn classifier with the available data.
def training_kkn_class_for_inputting(X,y, max_k):
    X_data_Knn_train, X_data_Knn_test, y_data_Knn_train, y_data_Knn_test = train_test_split(X, y, random_state = 0)
    n = []
    classifiers = []
    scores = []
    for i in range(1,max_k):
        knn = KNeighborsClassifier(n_neighbors = i).fit(X_data_Knn_train, y_data_Knn_train)
        accuracy = knn.score(X_data_Knn_test, y_data_Knn_test)
        #Append values.
        n.append(i)
        classifiers.append(knn)
        scores.append(accuracy)
    
    #Create df with data.
    data = {'n':n,'classifier':classifiers,'accuracy_values':scores}
    df = pd.DataFrame(data)
    
    #Obtain best classifier results.
    best_acc = df['accuracy_values'].max()
    df_to_return = df[df['accuracy_values']==best_acc]
    
    #Obtain classifier
    classifier_sel = df_to_return['classifier'].values
    classifier_sel = classifier_sel[0]
    return classifier_sel

#Defining a method to predict a additives_n value given a vector (1,n_features) with a knn classiifier.
def predic_value_knn_classifier_inputting(X, knn_classifier):
    pred = knn_classifier.predict(X)
    pred = pred[0]
    return pred


#Defining a method to fill missing values in the additives_n column of a dataframe.
def filling_missing_values_additives_with_knn_classifier(df, knn_classifier):
    df_to_mod = df
    col_features = obtain_g_columns(df_to_mod)
    
    for idx, row in df_to_mod.iterrows():
        x = row[col_features].to_frame().T
        row_val = row['additives_n']
        val = predic_value_knn_classifier_inputting(x,knn_classifier)
        if(np.isnan(row_val)):
            df_to_mod.loc[idx,'additives_n'] = val
        
    return df_to_mod


#Defining a function to delete the aberrants values in a dataframe. 
#col_condition; column to check the conditions for aberrants.
#val_condition; boundary to check if values are aberrants.
#The conditions are checked for the following columns: 'fat_100g','sodium_100g','sugars_100g'
def df_deleting_aberrants(df, col_condition, val_condition, list_searched_words, verbose =False):
    idx = df[col_condition].to_frame()
    idx = idx[idx[col_condition]>=val_condition]
    idx = idx.index.tolist()
    df_too_much_product = df[df.index.isin(idx)]
    
    #Calculate indexes of products that aren't equal to the search passed by parameter, and contain too much of this component.
    searchfor = list_searched_words
    contains_product = df_too_much_product['product_name'].str.contains('|'.join(searchfor))
    df_not_product = df_too_much_product[contains_product==False]
    idx_not_product = df_not_product.index.tolist()
    df_products_to_delete = df[df.index.isin(idx_not_product)]
    df_to_return = df.drop(idx_not_product)
    df_to_return = df_to_return.reset_index(drop=True)
    
    if verbose == True:
        print('Products to be removed:')
        print(df_products_to_delete[['product_name', col_condition]])
    return df_to_return


#Defining methods to make univariate analysis visualizations in categorical variables.
def bar_plot_categorical_columns(df, col_name, plot_num):
    info = df[col_name].value_counts()
    info = info.reset_index()
    info.columns = ["values",'count']
    info = info.sort_values(by=['count'], ascending=False)
    info = info.iloc[0:20,:]
    plt.figure(plot_num)
    ax = info.plot.bar(x='values', y='count', rot=90, color = 'yellow')
    plt.title(col_name+"'s "+"frequency values")
    plt.xlabel('Categories')
    plt.ylabel('# values')

def table_categorical_columns(df, col_name, plot_num):
    info = df[col_name].value_counts()
    info = info.reset_index()
    info.columns = ["values",'count']
    print('Variable of analysis : '+col_name)
    print("Total number of variable values : "  +str(info.shape[0]))
    print("")
    print(info.head(20))
    print("")
    
    
def plots_univariate_analysis_categorical_variables(df_cat):
    cont = 0
    cols = list(df_cat.columns)
    #cols.remove('Unnamed: 0')
    #cols.remove('countries')
    #cols.remove('countries_en')
    cols.remove('countries_tags')
    cols.remove('code')
    cols.remove('product_name')
    cols.remove('ingredients_text')
    
    for i in cols:
        cont = cont+1
        bar_plot_categorical_columns(df_cat, i, cont)
                              
    
def tables_univariate_analysis_categorical_variables(df_cat):
    cont = 0
    cols = list(df_cat.columns)
    #cols.remove('Unnamed: 0')
    #cols.remove('countries')
    #cols.remove('countries_en')
    cols.remove('countries_tags')
    cols.remove('code')
    cols.remove('product_name')
    cols.remove('ingredients_text')
    
    for i in cols:
        cont = cont+1
        table_categorical_columns(df_cat, i, cont)
        
        
def pie_chart_categorical_columns(df_categorical, col_name, plot_num):
    table = pd.crosstab(index = df_categorical[col_name], columns="count")
    prop = table/table.sum()
    prop = prop.sort_values(["count"], ascending=False)
    prop.index.name = None
    prop.columns.name = None
    prop = prop.iloc[0:9,:]
    rest = 1-prop['count'].sum()
    lin = pd.DataFrame({"count": rest},index=[0])
    lin.index = ['Rest']
    prop = pd.concat([prop, lin], axis =0)
    #Make pie-chart.
    sns.set_theme(style="whitegrid")
    plt.figure(plot_num)
    pie_plot = prop.plot.pie(y='count', figsize=(5, 5), labels = None)
    #pie_plot = prop.plot.pie(y='count', figsize=(5, 5), autopct='%.2f')
    #pie_plot = proportion.plot.pie(y='count', figsize=(5, 5), autopct='%.2f')
    pie_plot.legend(loc='center left', bbox_to_anchor=(1, 0.5) ,labels=prop.index)
    pie_plot.set_title('Pie chart '+col_name, fontweight ="bold")
    
def univariate_analysis_pie_chart_categorical_columns(df_categorical):
    cols = list(df_categorical.columns)
    cols.remove('countries_tags')
    cols.remove('code')
    cols.remove('product_name')
    cols.remove('ingredients_text')
    idx= 0
    for i in cols:
        idx = idx+1
        pie_chart_categorical_columns(df_categorical, i, idx)
        
                
#Defining methods to make visualizations for univariate analisis in real variables.
#Display box plots for real variables.
#print(len(df_real_columns))
def plot_box_plots_real_variables(df):
    cols = df.columns
    for i in cols: 
        plt.figure(i)
        sns.set_theme(style="whitegrid")
        sns.boxplot(  y=i, data=df,  orient='v' ,color='yellow')
        #ax = sns.boxplot(y=i, data=df)
        plt.title("Open food facts's "+i+" "+"box plot")
        plt.xlabel(i)
        plt.xticks(rotation=90)
        plt.ylabel('Values '+i)
        
        
#Create histograms for real variables.
#Define a function that plots an histogram given an indicator series.
def plot_column_histogram(df_real, col_name, i_fig, n_bins =100):
    
    df_to_mod = df_real[col_name] 
    df_to_mod = df_to_mod.to_numpy()
    df_to_mod = df_to_mod[df_to_mod != 0]
    plt.figure(i_fig,figsize=(3,3))
    plt.hist(df_to_mod, density=True, bins=n_bins, color = "yellow")
    #plt.suptitle('Histogram : '+ind_name, fontsize=16, fontweight="bold")
    plt.xlabel('Indicator_values')
    plt.title("Histogram of "+col_name)
    plt.ylabel('Density values '+col_name)
    
def plot_histograms_real_variables(df):
    df_to_mod = df
    cols = df.columns
    con = 0
    for i in cols: 
        con = con + 1
        plot_column_histogram(df_to_mod, i, con)
        
        
#Defining methods to avoid the outlayers within the real variables of the dataframe.
def remove_outlayers_base_on_column(df, column_name):
    df_to_mod = df
    variable = df_to_mod[column_name]
    q1 = variable.quantile(0.25)
    q3 = variable.quantile(0.75)
    IQR = q3- q1
    df_to_mod = df_to_mod[~((variable < (q1 - 1.5 * IQR)) |(variable > (q3 + 1.5 * IQR)))]
    return df_to_mod


def remove_outlayers_in_df(df):
    df_to_mod = df
    cols = list(df_to_mod.columns)
    
    for i in cols:
        #print(i)
        #print(df_to_mod.shape)
        df_to_mod = remove_outlayers_base_on_column(df_to_mod,i)
    return df_to_mod



def update_df_part_shape(df_part, idx):
    df_part = df_part[df_part.index.isin(idx)]
    return df_part


#Defining methods to do univariate analysis.

#Defining a method to obtain data from different categories in "main_category_en".
def concatenate_df_parts(df_part_1, df_part_2):
    df_to_return = pd.concat([df_part_1, df_part_2], axis=1)
    return df_to_return


def obtain_dfs_vals_for_categories_in_column(df, column_name, score_column_name):
    categories = list(df[column_name].unique())
    columns = []
    categories_vals = []
    for i in categories:
        condition =  df[column_name]==i
        subset_df = df[condition]
        subset_df_column = subset_df[score_column_name].values
        columns.append(i)
        categories_vals.append(subset_df_column)
        
    dict_vals = dict(zip(columns,categories_vals)) 
    return dict_vals

def box_plots_real_variable_categories_feature(dict_values,start,with_size):
    keys = list(dict_values.keys())
    array = list(dict_values.values())
    for i in range(start,start+with_size):
        bp = boxplot(array[i], positions= [i+1], widths = 0.6)
        for box in bp['boxes']:
        # change outline color
            box.set(color='yellow', linewidth=2)
    plt.title("Box plots' nutrition score and product categories ",fontsize=20, fontweight='bold')
    plt.xlabel('Categories',)
    plt.ylabel('Nutrition score')
    _= plt.xticks(range(start+1,start+with_size+1),keys[start:start+with_size],rotation=90)





#Implementing a function that allows to create box plots for real features..
def plot_box_plots_real_variables(df):
    cols = df.columns
    for i in cols: 
        plt.figure(i)
        sns.set_theme(style="whitegrid")
        sns.boxplot(  y=i, data=df,  orient='v')
        #ax = sns.boxplot(y=i, data=df)
        plt.title("Wine's exercise: "+i+" "+"box plot")
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
    plt.hist(df_to_mod, density=True, bins=n_bins)
    #plt.suptitle('Histogram : '+ind_name, fontsize=16, fontweight="bold")
    plt.xlabel(col_name+' values')
    plt.title("Wine's exercise"+ "Histogram of "+col_name)
    plt.ylabel(col_name+" density's")
    
#Defining a method that create histograms for real variables in a dataset.
def plot_histograms_real_variables(df):
    df_to_mod = df
    cols = df.columns
    con = 0
    for i in cols: 
        con = con + 1
        plot_column_histogram(df_to_mod, i, con)
        
#Defining a method that removes outlayers from a dataframe column.       
def remove_outlayers_base_on_column(df, column_name):
    df_to_mod = df
    variable = df_to_mod[column_name]
    q1 = variable.quantile(0.25)
    q3 = variable.quantile(0.75)
    IQR = q3- q1
    df_to_mod = df_to_mod[~((variable < (q1 - 1.5 * IQR)) |(variable > (q3 + 1.5 * IQR)))]
    return df_to_mod

#Defining a method that removes outlayers from all columns in a  dataframe.    
def remove_outlayers_in_df(df):
    df_to_mod = df
    cols = list(df_to_mod.columns)
    
    for i in cols:
        df_to_mod = remove_outlayers_base_on_column(df_to_mod,i)
    
    return df_to_mod

#Define a method that creates bar plots for categorical variables.
def bar_plot_categorical_columns(df, col_name, plot_num):
    info = df[col_name].value_counts()
    info = info.reset_index()
    info.columns = ["values",'count']
    info = info.sort_values(by=['values'], ascending=True)
    info = info.iloc[0:20,:]
    plt.figure(plot_num)
    ax = info.plot.bar(x='values', y='count')
    plt.title(col_name+"'s "+"frequency values")
    plt.xlabel('Categories')
    plt.ylabel('# values')

#Define a method that creates a df table exposing values by categories in a column.
def table_categorical_columns(df, col_name, plot_num):
    info = df[col_name].value_counts()
    info = info.reset_index()
    info.columns = ["values",'count']
    print('Variable of analysis : '+col_name)
    print("Total number of variable values : "  +str(info.shape[0]))
    print("")
    print(info.head(20))
    print("")
    
#Define a method that builds a colorful correlation matrix table.    
def create_corrMatrix(X):
    corrMatrix = X.corr()
    fig, ax = plt.subplots(figsize=(10,10))         # Sample figsize in inches
    sns.heatmap(corrMatrix, annot=True, linewidths=.5, ax=ax)
    plt.title("Correlation matrix",fontsize=20, fontweight='bold')
    plt.xlabel('Real variables',)
    plt.ylabel('Real variables')
    plt.figure(figsize=(10,10))
    plt.show()
    
#Create a method that calcualtes the information from features and the target variable.
def giving_mutual_info(X_val, y_val):
    mutual_info = mutual_info_classif(X_val, y_val) 
    mutual_info = pd.Series(mutual_info)
    mutual_info.index = X_val.columns
    mutual_info = mutual_info.sort_values(ascending=False)
    return mutual_info
    

#Defining a method that calculates the percentage of null values in a dataframe.
#Returns a dataframe having ratio of null values per column.
def cal_nan_percents(dask_df):
    percent_missing = dask_df.isnull().sum() / len(dask_df)
    missing_value_df = pd.DataFrame({'column_name': dask_df.columns,'percent_missing': percent_missing})
    percent_missing = pd.Series(percent_missing).to_frame()
    percent_missing.columns = ["Nan_percent"]
    columns = pd.DataFrame(dask_df.columns,columns=['columns_names'])
    df_nan_percents = pd.concat([columns,percent_missing],axis=1)
    return df_nan_percents


#Cleaning phase.
#Defining a method to create binary columns for ingredients .
def create_binary_columns_for_heart_health_products(df, heart_healthy_foods_list):
    df_to_mod = df
    for i in heart_healthy_foods_list:
        a = i.lower()
        df_to_mod['contains_'+i] = df_to_mod['ingredients_text'].str.contains(a)
        df_to_mod['contains_'+i] = df_to_mod['contains_'+i].astype(int)
    return(df_to_mod)

#Defining methods to count the number of health ingredients in a product.
#Count the number of hear-healthy composals in each product.
def obtain_binary_columns_heart_healthy_foods_in_df(df):
    df_mod = df
    binary_columns = []
    for i in list(df_mod.columns):
        if(i[0:9] == 'contains_'):
            binary_columns.append(i)
            
    return binary_columns


def obtain_columns_heart_healthy_foods(array_heart_healthy_foods):
    arr_to_mod = array_heart_healthy_foods
    arr = []
    for i in arr_to_mod:
        arr.append('contains_'+i)
    return(arr)

def sum_heart_healthy_foods_per_product(df,heart_healthy_foods_columns):
    df_to_mod = df[heart_healthy_foods_columns]
    colums_sum = df_to_mod.sum(axis = 1, skipna = True)
    colums_sum = colums_sum.to_frame()
    colums_sum.columns = ['sum_healthy_composals']
    return colums_sum

def add_sum_heart_healthy_foods_per_product_to_df(df, heart_healthy_foods_ingredients):
    df_sum = sum_heart_healthy_foods_per_product(df,obtain_columns_heart_healthy_foods(heart_healthy_foods_ingredients))
    df_to_return = dd.concat([df, df_sum], axis=1)
    return df_to_return

#Defining methods to check wether the product's information is comming from france or not.
def is_product_sold_in_france(df):
    countries_column = ['countries','countries_tags','countries_en']
    df_countries_column = df[countries_column]
    df_countries_column = df_countries_column['countries_en'].unique().compute()   
    s = df_countries_column    
    contain_france = []
    for index, value in s.items():
        a = False
        try:
            if("France" in value):
                a = True
        except:
            a = a 
                
        contain_france.append(a)
    
    df = pd.DataFrame(contain_france,columns=['in_france'])
    s = s.to_frame()
    
    df_to_return = pd.concat([s,df],axis=1)
    return df_to_return

def create_mapping_is_product_in_france(df, df_is_product_sold_in_france):
    equiv = dict(list(zip(df_is_product_sold_in_france["countries_en"], df_is_product_sold_in_france["in_france"])))
    df["in_france"] = df["countries_en"].map(equiv)
    return df


#Defining methods to assure sufficient information within the dataframe.
def obtain_g_columns(df):
    df_mod = df
    g_columns = []
    
    for i in df_mod.columns:
        if (i[-5:] == '_100g'):
            g_columns.append(i)
            
    return g_columns 

#If the columns _100g are not empty, assign the value of one. zero otherwise.
def obtain_vectors_100g(df):
    df_mod = df
    columns = obtain_g_columns(df)
    vectors = []
    n_vectors = 0
    
    for i in columns:
        n_vectors = n_vectors+1
        v = df_mod[i] == df_mod[i]
        v = v.values*1 
        vectors.append(v)
        
    return vectors , n_vectors

def sum_vectors_100g(df):
    vectors, n_vectors = obtain_vectors_100g(df)
    c = vectors[0]
    for i in vectors[1:]:
        c = c + i
    c = dd.from_dask_array(c)
    c = c.to_frame()
    c.columns = ['num_100g_non_empty_columns']
    return c, n_vectors

#There are 100 columns finishing by 100mg in the database.
def delete_rows_empty_100g_columns(df, min_num_mg_cols):
    df_to_modify = df
    sum_vec, n_vectors_sum = sum_vectors_100g(df_to_modify)
    sum_vec.index = df_to_modify.index
    df_to_modify = dd.concat([df_to_modify,sum_vec],axis =1)
    df_to_modify = df_to_modify[(df_to_modify['num_100g_non_empty_columns'] >= min_num_mg_cols)]
    return df_to_modify

#Defining a method to remove 100g_columns having all values as null.
def return_100_mg_nan_columns(df_columns_nan_percents, list_mg_100_cols):
    df = df_columns_nan_percents
    list_columns = []   
    df = df[df['columns_names'].isin(list_mg_100_cols)==True]    
    for i, j in df.iterrows():
        col_name = j['columns_names'] 
        col_nan_percent = j['Nan_percent']
    
        if (col_nan_percent == 1):
            list_columns.append(col_name)
            
    return list_columns

#Defining a method that returns a list with the columns having no more than certain amount of values.
def return_almost_and_null_nan_columns(df_columns_nan_percents,list_mg_100_cols , threshold):
    df_to_mod = df_columns_nan_percents
    df_to_mod_cp = df_columns_nan_percents
    
    #Iterate over columns that do not finish by '100_g'.
    df_to_mod = df_to_mod[df_to_mod['columns_names'].isin(list_mg_100_cols)==False]    
    list_normal_columns = []
    for i, j in df_to_mod.iterrows():
        col_name = j['columns_names'] 
        col_nan_percent = j['Nan_percent']
    
        if (col_nan_percent >= threshold):
            list_normal_columns.append(col_name)
    
    list_columns_100_g_nan = return_100_mg_nan_columns(df_to_mod_cp, list_mg_100_cols)
    
    return list_normal_columns, list_columns_100_g_nan


def remove_almost_and_null_columns(df, list_almost_empty_columns, list_columns_100_g_nan):
    df_mod = df
    conc_list = list_almost_empty_columns + list_columns_100_g_nan
    df_mod = df_mod.drop(labels=conc_list, axis=1)
        
    return df_mod


def obtain_proportion_healthy_composals(df):
    df_to_mod = df.compute()
    df_to_mod["proportion_health_composals"] = df_to_mod["ingredients_text"].str.split(", ")
    for idx, row in df_to_mod.iterrows():
        df_to_mod.loc[idx,'proportion_health_composals'] = row['sum_healthy_composals']/len(row['proportion_health_composals'])
    
    df_to_mod = dd.from_pandas(df_to_mod, npartitions=3)
    return df_to_mod

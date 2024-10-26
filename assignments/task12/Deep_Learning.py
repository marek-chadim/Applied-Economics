# %% [markdown]
# ## Easy record-linkage using the LinkTransformer libary
# 
# From https://colab.research.google.com/drive/1OqUB8sqpUvrnC8oa_1RoOUzV6DaAKL4N?usp=sharing#scrollTo=t0BTOG2bqeIQ

# %% [markdown]
# ## Setup

# %%
# An environment with python=3.11 works. (3.12 does not work.)

#!pip install linktransformer

# %%
import linktransformer as lt
import pandas as pd
import os


# %%
os.chdir('C:/Dropbox/Applied-Economics/task12')

# %% [markdown]
# # Supported Pre-trained models
# 
# We support several LinkedTransformer models, check out our guide on selecting the best models on this tutorial notebook [model zoo](https://colab.research.google.com/drive/1SAvQdgYiX2CinoTC8Y5qtKScNwx3DYXf?usp=sharing). Our framwork supports all language models on [HuggingFace](https://huggingface.co/models?pipeline_tag=sentence-similarity&sort=trending). We highly recommend the sentence transformers models also available on HuggingFace. For more on sentence-transformers, visit their [website](https://www.sbert.net/)
# 
# For users of LinkedTransformer who are new to NLP, here is a guide we have prepared for you to select an appropriate semantic similarity model. (https://colab.research.google.com/drive/1SAvQdgYiX2CinoTC8Y5qtKScNwx3DYXf?usp=sharing)
# 
# For the use case of linking companies and harmonising trade products (applications covered in our paper), here is a tutorial on how to do it with a high level interface, check out our package [repo](https://github.com/dell-research-harvard/linktransformer)

# %% [markdown]
# ## Standard (deep) merge on a key
# 
# Just like you would in any merge command!

# %%
###Read data
df1 = pd.read_csv("toy_comp_1.csv")
df2 = pd.read_csv("toy_comp_2.csv")

df1.head(3)

# %%
import linktransformer as lt


# Test your function here
df_lm_matched = lt.merge(df2, df1, merge_type='1:m', on="CompanyName", model="all-MiniLM-L6-v2", left_on=None, right_on=None)

df_lm_matched.head(10)



# %%
df_lm_matched[["CompanyName_x","CompanyName_y","score","Revenue (Millions USD)","Num_Employees"]]

# %% [markdown]
# ## Merge with Blocking - easily in a line
# 
# Often it is necessary to merge within partitions of datasets using a "blocking" variable. In this case, we merge "CompanyName" with only companies that have the same "Country" value.
# 

# %%
df1 = pd.read_csv("toy_comp_1.csv")
df2 = pd.read_csv("toy_comp_2.csv")

# Test your function here
df_lm_matched = lt.merge_blocking(df2, df1, merge_type='1:m', on="CompanyName", model="all-MiniLM-L6-v2", left_on=None, right_on=None, blocking_vars=["Country"])
df_lm_matched.head(10)


# %% [markdown]
# ## All functions also support multiple keys!
# 
# Matching over multiple keys is also supported, for example, CompanyName and ProductDescription. Multiple columns are serialized by concatenating with a <SEP> token that the package selects to be suitable with the base language model tokenizer.

# %%
df1 = pd.read_csv("toy_multi_1.csv")
df2 = pd.read_csv("toy_multi_2.csv")

df1.head(3)

# %%
# Test your function here
df_lm_matched = lt.merge(df2, df1, merge_type='1:m', on=["CompanyName", "ProductDescription"], model="all-MiniLM-L6-v2", left_on=None, right_on=None)

df_lm_matched.head(10)

# %% [markdown]
# ## Quick Aggregation or Classification as KNN retrieval
# A merge can be used to aggregate or classify (KNN) - say fine product descriptions to coarser categories.

# %%
df_coarse = pd.read_csv("coarse.csv")
df_fine = pd.read_csv("fine.csv")

# %%

df_lm_aggregate = lt.aggregate_rows(df_fine, df_coarse, model="all-mpnet-base-v2", left_on="Fine Category Name", right_on="Coarse Category Name")

df_lm_aggregate.head()


# %%
df_lm_aggregate[["Fine Category Name","Coarse Category Name"]].head(10)

# %% [markdown]
# ## Bypass translation with Cross-lingual merges
# You can merge across languages - without having the need to translate anything! Just use a multilingual model to merge.

# %%
df_french = pd.read_csv("translation_1.csv")
df_english = pd.read_csv("translation_2.csv")

df_french.head(3)

# %%
# French to English Merge


# Test your function here
df_lm_matched = lt.merge(df_french, df_english, merge_type='1:m', left_on="Libellé du Produit", right_on="Product Label", model="distiluse-base-multilingual-cased-v1")

df_lm_matched.head(10)


# %%
df_lm_matched[["Libellé du Produit","Product Label","Catégorie","Category"]].head(10)

# %% [markdown]
# ## Perform clustering at the row level in one line!
# 
# Several algorithms are supported and more to be added soon.
# 

# %%
df=pd.read_csv("toy_comp_2.csv")
df_cluster=lt.cluster_rows(df,on="CompanyName",model="sentence-transformers/all-MiniLM-L6-v2",cluster_type= "agglomerative",
    cluster_params= {'threshold': 0.7})
df_cluster.head(10)

# %% [markdown]
# ## Deduplicate your rows in one line!
# Deduplicate using a language model with one line of code! Several clustering algorithms like SLINK, HDBSCAN and DBSCAN are supported

# %%
df=pd.read_csv("toy_comp_2.csv")
df_dedup=lt.dedup_rows(df,on="CompanyName",model="sentence-transformers/all-MiniLM-L6-v2",cluster_type= "agglomerative",
    cluster_params= {'threshold': 0.7})
df_dedup.head(10)

# %%
df.head(10)

# %% [markdown]
# ## We also support getting semantic similarity scores between values of specified columns!
# All you need is a dataframe containing the columns of interest as keys, and you are done! This will be a very powerful drop in replacement for edit distance calculations

# %%
df=pd.read_csv("toy_pairs.csv")
df.head(3)

# %%

df_eval=lt.evaluate_pairs(df, model="sentence-transformers/all-MiniLM-L6-v2",left_on="company_name_1",right_on="company_name_2",openai_key=None)
df_eval.head()

# %% [markdown]
# ## Get all pair-wise distances between keys from two datasets

# %%
df=pd.read_csv("toy_pairs.csv")
df_eval=lt.all_pair_combos_evaluate(df, model="sentence-transformers/all-MiniLM-L6-v2",left_on="company_name_1",right_on="company_name_2",openai_key=None)
df_eval.head(100)

# %% [markdown]
# ## Get K nearest matches for each row in the left df

# %%
df1 = pd.read_csv("toy_comp_1.csv")
df2 = pd.read_csv("toy_comp_2.csv")

df_lm_matched_2nn = lt.merge_knn(df2, df1, on="CompanyName", model="sentence-transformers/all-MiniLM-L6-v2", left_on=None, right_on=None,
                                k=2)

df_lm_matched_2nn.head()

# %%
df_lm_matched = lt.merge(df1, df2, merge_type='1:m', on="CompanyName", model="dell-research-harvard/lt-wikidata-comp-en", left_on=None, right_on=None)

df_lm_matched.head(10)

# %%
#save
df_lm_matched.to_csv("toy_comp_matched.csv",index=False)



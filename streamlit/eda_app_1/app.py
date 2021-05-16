
# We install streamlit from conda-forge
# https://anaconda.org/conda-forge/streamlit

# We also install pandas-profiling from conda-forge
# https://github.com/pandas-profiling/pandas-profiling
# Had to install a specific version of pandas-profiling == 2.8.0 to get it to work.
# https://github.com/pandas-profiling/pandas-profiling/issues/528

# Dataset gotten from UCI ML repository
# http://archive.ics.uci.edu/ml/datasets/AI4I+2020+Predictive+Maintenance+Dataset


# Import libraries here
import numpy as np
import pandas as pd
import streamlit as st
from pandas_profiling import ProfileReport
from streamlit_pandas_profiling import st_profile_report

st.set_page_config(layout='wide')

st.markdown('''
# ** The EDA App V1**

This is **V1 of the EDA App** created in Streamlit using the **pandas-profiling** library.

**Credit:** App built in `Python` + `Streamlit` by [Vincent Ng Wee Kien](https://www.linkedin.com/in/ngweekiensg/)

---
''')

# Upload CSV data
with st.sidebar.header('1. Upload CSV data here:'):
    uploaded_file = st.sidebar.file_uploader("Upload input CSV file", type=["csv"])
    st.sidebar.markdown("""
    [Example CSV input file](http://archive.ics.uci.edu/ml/machine-learning-databases/00601/ai4i2020.csv)
""")

# Pandas Profiling Report
if uploaded_file is not None:
    @st.cache
    def load_csv():
        csv = pd.read_csv(uploaded_file)
        return csv
    df = load_csv()
    pr = ProfileReport(df, explorative=True)
    st.header('**Input DataFrame**')
    st.write(df)
    st.write('---')
    st.header('**Pandas Profiling Report**')
    st_profile_report(pr)
else:
    st.info('Awaiting for CSV file to be uploaded.')
    if st.button('Press to use Example Dataset'):
        # Example data
        @st.cache
        def load_data():
            a = pd.DataFrame(
                np.random.rand(100, 7),
                columns=['a', 'b', 'c', 'd', 'e', 'f', 'g']
            )
            return a
        df = load_data()
        pr = ProfileReport(df, explorative=True)
        st.header('**Input DataFrame**')
        st.write(df)
        st.write('---')
        st.header('**Pandas Profiling Report**')
        st_profile_report(pr)










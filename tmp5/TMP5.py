import streamlit as st
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns

from sklearn.model_selection import train_test_split
from sklearn.ensemble import RandomForestClassifier
from sklearn.metrics import classification_report

st.title("Wine Quality Viewer")

def data():
    Red = pd.read_csv("winequality-red.csv", sep=";")
    White = pd.read_csv("winequality-white.csv", sep=";")
    Red["color"] = "Red"
    White["color"] = "White"
    return pd.concat([Red, White], ignore_index=True)

df = data()

tab_metadata, tab_data, tab_color, tab_quality, tab_scatter, tab_box, tab_classification = st.tabs(
    ["MetaData", "Data", "Color", "Quality", "Scatter", "Box", "Classification"]
)

with tab_metadata:
    st.header("Meta Data")

    st.subheader("Dataset Overview")
    st.write(f"Number of rows: {df.shape[0]}")
    st.write(f"Number of columns: {df.shape[1]}")

    st.subheader("Column Information")

    metadata_df = pd.DataFrame({
        "name": df.columns,
        "role": ["Feature"] * len(df.columns),
        "type": ["Continuous" if pd.api.types.is_numeric_dtype(df[col]) else "Categorical"
                 for col in df.columns],
        "demographic": ["None"] * len(df.columns),
        "description": ["None"] * len(df.columns),
        "units": ["None"] * len(df.columns),
        "missing_values": ["yes" if df[col].isnull().any() else "no"
                           for col in df.columns]
    })

    st.dataframe(metadata_df, use_container_width=True)

with tab_data:
    st.header("Data Set Viewer")

    selected_view = st.radio(
        "Choose dataset view",
        ["Chemical", "Alcohol", "Quality", "All"]
    )

    chemical_columns = [
        "color",
        "fixed acidity",
        "volatile acidity",
        "citric acid",
        "residual sugar",
        "chlorides",
        "free sulfur dioxide",
        "total sulfur dioxide",
        "density",
        "pH",
        "sulphates"
    ]

    alcohol_columns = ["color", "alcohol"]
    quality_columns = ["color", "quality"]
    all_columns = ["color"] + [col for col in df.columns if col != "color"]

    if selected_view == "Chemical":
        st.dataframe(df[chemical_columns], use_container_width=True)

    elif selected_view == "Alcohol":
        st.dataframe(df[alcohol_columns], use_container_width=True)

    elif selected_view == "Quality":
        st.dataframe(df[quality_columns], use_container_width=True)

    elif selected_view == "All":
        st.dataframe(df[all_columns], use_container_width=True)

with tab_color:
    st.header("Color Distribution")

    color_counts = df["color"].value_counts().reindex(["Red", "White"], fill_value=0)

    fig, ax = plt.subplots(figsize=(8, 5))
    ax.bar(color_counts.index, color_counts.values)
    ax.set_xlabel("Wine Color")
    ax.set_ylabel("Number of Wines")
    ax.set_title("Number of Wines by Color")
    st.pyplot(fig)

with tab_quality:
    st.header("Quality Distribution")

    quality_counts = df["quality"].value_counts().sort_index()

    quality_range = list(range(3, 10))
    quality_counts = quality_counts.reindex(quality_range, fill_value=0)

    fig, ax = plt.subplots(figsize=(8, 5))
    ax.bar([str(q) for q in quality_counts.index], quality_counts.values)
    ax.set_xlabel("Quality Rating")
    ax.set_ylabel("Number of Wines")
    ax.set_title("Number of Wines by Quality")
    st.pyplot(fig)

with tab_scatter:
    st.header("Scatter Plots")
    numeric_cols = df.select_dtypes(include="number").columns.tolist()
    x_axis = st.selectbox(
        "Select X-axis Variable",
        options=[None] + numeric_cols,
        format_func=lambda x: "Select a variable" if x is None else x
    )
    y_axis = st.selectbox(
        "Select Y-axis Variable",
        options=[None] + numeric_cols,
        format_func=lambda x: "Select a variable" if x is None else x
    )
    if x_axis and y_axis:
        fig, ax = plt.subplots()
        sns.scatterplot(data=df, x=x_axis, y=y_axis, hue="color", ax=ax)
        st.pyplot(fig)
    else:
        st.info("Select the variables to display the scatter plot.")

with tab_box:
    st.header("Box Plots")

    numeric_cols = df.select_dtypes(include="number").columns.tolist()
    selected_col = st.radio("Choose a Variable to Plot", numeric_cols)

    wine_type = st.radio("Select Wine Type", ["Both", "Red", "White"])

    if wine_type == "Red":
        filtered_df = df[df["color"] == "Red"]
    elif wine_type == "White":
        filtered_df = df[df["color"] == "White"]
    else:
        filtered_df = df

    fig, ax = plt.subplots()
    sns.boxplot(y=filtered_df[selected_col], ax=ax)

    ax.set_title(f"Distribution of {selected_col} ({wine_type} Wine)")
    st.pyplot(fig)

with tab_classification:
    st.header("Classification")

    class_df = df.copy()
    class_df["color"] = class_df["color"].replace({"Red": 0, "White": 1})

    X = class_df.drop("quality", axis=1)
    y = class_df["quality"]

    X_train, X_test, y_train, y_test = train_test_split(
        X, y,
        test_size=0.2,
        random_state=42,
        stratify=y
    )

    model = RandomForestClassifier(
        n_estimators=100,
        max_depth=10,
        random_state=42
    )

    model.fit(X_train, y_train)
    y_pred = model.predict(X_test)

    st.subheader("Classification Report")

    report_dict = classification_report(y_test, y_pred, output_dict=True, zero_division=0)
    report_df = pd.DataFrame(report_dict).transpose()

    st.dataframe(report_df)

    st.subheader("Feature Importance")

    importance_df = pd.DataFrame({
        "Feature": X.columns,
        "Importance": model.feature_importances_
    }).sort_values("Importance", ascending=False)

    st.dataframe(importance_df)

    fig, ax = plt.subplots()
    ax.bar(importance_df["Feature"], importance_df["Importance"])
    ax.set_title("Feature Importance")
    plt.xticks(rotation=45, ha="right")

    st.pyplot(fig)
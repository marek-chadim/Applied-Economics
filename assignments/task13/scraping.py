# %% [markdown]
# # Webscraping

# %% [markdown]
# [Numbeo](https://www.numbeo.com/cost-of-living/), is a website where users supply price information in different locations. The task is to get the
# cost of living in USD($) for all countries. Get the name of each product category, the price of the product
# in USD. Also get the cost of living relative to the United States. Save this together with the country
# names in a csv file

# %% [markdown]
# 1. Use the request module to download and save as .htm https://www.numbeo.com/cost-of-
# living/ .

# %%
import requests
from bs4 import BeautifulSoup
import pandas as pd
import re
import time
import os

# Step 1: Download and save the main cost of living page
base_url = "https://www.numbeo.com/cost-of-living/"
response = requests.get(base_url)

# Save the main page as HTML
main_page_path = "numbeo_main_page.htm"
with open(main_page_path, "w", encoding="utf-8") as file:
    file.write(response.text)

# %% [markdown]
# 2. Load the file into a Beautiful Soup object. Find all possible country input values. You can see
# all parameters in the URL, for example, https://www.numbeo.com/cost-of-living/
# country_result.jsp?country=Sweden&displayCurrency=USD. This means that you can just
# loop over country names in the url.

# %%
# Step 2: Load the file into BeautifulSoup and find all country input values
with open(main_page_path, "r", encoding="utf-8") as file:
    soup = BeautifulSoup(file, "html.parser")

# Extract all countries from dropdown
countries = [option.get_text(strip=True) for option in soup.select("select[name='country'] option")]
print("Countries found:", countries)
# Extract all countries from the dropdown menu, filtering out placeholder options
countries = [option.get_text(strip=True) for option in soup.select("select[name='country'] option") if option.get_text(strip=True) not in ["---Select country---"]]
print("Filtered Countries:", countries)


# Base URL for country-specific pages
country_base_url = "https://www.numbeo.com/cost-of-living/country_result.jsp?country={}&displayCurrency=USD"

# Directory to save country files
output_dir = "country_data"
os.makedirs(output_dir, exist_ok=True)

# Initialize list to store data for all countries
all_country_data = []

# %% [markdown]
# 3. Download and save one country file. Load the file into a Beautiful soup object. Find the
# relevant table. Print the table headings and values, together with the country name to a file.
# Then use regular expressions to extract the cost of living relative to the Sweden. Add this to the
# printed table.

# %%
#Step 3: Test with Czech Republic
for country in countries:
    if country != "Czech Republic":
        continue  # Skip all countries except Czech Republic
    
    # Format the URL
    country_url = country_base_url.format(country.replace(" ", "+"))
    country_response = requests.get(country_url)
    
    # Save the Czech Republic page as an HTML file
    country_file_path = os.path.join(output_dir, f"{country}_cost_of_living.htm")
    with open(country_file_path, "w", encoding="utf-8") as file:
        file.write(country_response.text)

    # Parse the country file
    country_soup = BeautifulSoup(country_response.content, "html.parser")
    table = country_soup.find("table", {"class": "data_wide_table"})
    
    # Extract table data for Czech Republic
    country_data = []
    if table:
        headers = [th.get_text(strip=True) for th in table.find_all("th") if th.get_text(strip=True) and th.get_text(strip=True) not in ["Edit", ""]]
        for row in table.find_all("tr"):
            cells = row.find_all("td")
            if len(cells) >= 2:
                category = cells[0].get_text(strip=True)
                price = cells[1].get_text(strip=True)
                country_data.append({"Country": country, "Category": category, "Price (USD)": price})
    
    # Extract cost of living index relative to Sweden
    summary_section = country_soup.find("div", class_="seeding-call table_color summary limit_size_ad_right padding_lower other_highlight_color")
    relative_cost_text = "N/A"
    if summary_section:
        relative_cost_match = re.search(r"Cost of living in.*is, on average, (\d+\.\d+%)\s*(lower|higher) than in Sweden", summary_section.get_text())
        if relative_cost_match:
            relative_cost_text = f"{relative_cost_match.group(1)} {relative_cost_match.group(2)}"
    
    # Add relative cost info to each entry
    for entry in country_data:
        entry["Relative Cost to Sweden"] = relative_cost_text
        
print(country_data)


# %% [markdown]
# 4. Loop over all countries and do step 3. Note that URLs never include whitespaces; therefore,
# check what happens when a country name consists of multiple words.

# %%
# Step 4: Loop over each country, download page, and parse cost of living data
for country in countries:
    country_url = country_base_url.format(country.replace(" ", "+"))
    country_response = requests.get(country_url)
    
    # Save country page as HTML file
    country_file_path = os.path.join(output_dir, f"{country}_cost_of_living.htm")
    with open(country_file_path, "w", encoding="utf-8") as file:
        file.write(country_response.text)

    # Parse the country file
    country_soup = BeautifulSoup(country_response.content, "html.parser")
    table = country_soup.find("table", {"class": "data_wide_table"})
    
    # Extract table data
    country_data = []
    if table:
        headers = [th.get_text(strip=True) for th in table.find_all("th") if th.get_text(strip=True) and th.get_text(strip=True) not in ["Edit", ""]]
        for row in table.find_all("tr"):
            cells = row.find_all("td")
            if len(cells) >= 2:
                category = cells[0].get_text(strip=True)
                price = cells[1].get_text(strip=True)
                country_data.append({"Country": country, "Category": category, "Price (USD)": price})
    
    # Extract cost of living index relative to Sweden
    summary_section = country_soup.find("div", class_="seeding-call table_color summary limit_size_ad_right padding_lower other_highlight_color")
    relative_cost_text = "N/A"
    if summary_section:
        relative_cost_match = re.search(r"Cost of living in.*is, on average, (\d+\.\d+%)\s*(lower|higher) than in Sweden", summary_section.get_text())
        if relative_cost_match:
            relative_cost_text = f"{relative_cost_match.group(1)} {relative_cost_match.group(2)}"
    
    # Add relative cost info to each entry for this country
    for entry in country_data:
        entry["Relative Cost to Sweden"] = relative_cost_text
    
    # Add this country's data to the all_country_data list
    all_country_data.extend(country_data)
    
    # Delay to avoid hitting the server too frequently
    time.sleep(1)

# %% [markdown]
# 5. Read all files into Stata

# %%
# Step 5: Save all data to a CSV file
output_csv = "numbeo_cost_of_living_all_countries.csv"
df = pd.DataFrame(all_country_data)
df.head()

# %%
# Replace non-numeric values '?' with NaN, and remove commas and currency symbols for conversion
df['Price (USD)'] = df['Price (USD)'].replace('[\$,]', '', regex=True).replace('?', None).astype(float)
# Cleaning and reformatting Relative Cost to Sweden column
relative_cost = df['Relative Cost to Sweden'].str.extract(r'(\d+\.?\d*)').astype(float)
sign = df['Relative Cost to Sweden'].str.contains("lower").map({True: -1, False: 1})
df['Relative Cost to Sweden'] = relative_cost[0] * sign

# Renaming the last column for clarity
df.rename(columns={'Relative Cost to Sweden': 'Relative Cost (%) to Sweden'}, inplace=True)

#save
df.to_csv(output_csv, index=False)

#showing the cleaned data
df.head()

# %%




# %%




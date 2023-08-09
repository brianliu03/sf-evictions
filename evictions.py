import pandas as pd

# Read the CSV file and only keep the relevant columns
evictions = pd.read_csv("sfevictionnotices.csv", usecols=["File Date", "Non Payment", "Breach", "Nuisance",
                                                    "Illegal Use", "Failure to Sign Renewal",
                                                    "Access Denial", "Unapproved Subtenant",
                                                    "Owner Move In", "Demolition", "Capital Improvement",
                                                    "Substantial Rehab", "Ellis Act WithDrawal",
                                                    "Condo Conversion", "Roommate Same Unit",
                                                    "Other Cause", "Late Payments",
                                                    "Lead Remediation", "Development",
                                                    "Good Samaritan Ends"])

# Reshape the DataFrame to long format
evictions_long = pd.melt(evictions, id_vars=["File Date"], value_vars=["Non Payment", "Breach", "Nuisance",
                                                                       "Illegal Use", "Failure to Sign Renewal",
                                                                       "Access Denial", "Unapproved Subtenant",
                                                                       "Owner Move In", "Demolition", "Capital Improvement",
                                                                       "Substantial Rehab", "Ellis Act WithDrawal",
                                                                       "Condo Conversion", "Roommate Same Unit",
                                                                       "Other Cause", "Late Payments",
                                                                       "Lead Remediation", "Development",
                                                                       "Good Samaritan Ends"],
                         var_name="Eviction Type", value_name="Is True")

# Filter the rows where the condition is met
filtered_evictions = evictions_long[evictions_long["Is True"]]

# Drop the "Is True" column
filtered_evictions.drop("Is True", axis=1, inplace=True)

# Display the resulting DataFrame
print(filtered_evictions)

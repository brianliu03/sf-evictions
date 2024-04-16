import pandas as pd

# Read the CSV file and only keep the relevant columns
evictions = pd.read_csv(
    "sfevictionnotices.csv",
    usecols=["File Date", "Non Payment", "Breach", "Nuisance",
            "Illegal Use", "Failure to Sign Renewal",
            "Access Denial", "Unapproved Subtenant",
            "Owner Move In", "Demolition", "Capital Improvement",
            "Substantial Rehab", "Ellis Act WithDrawal",
            "Condo Conversion", "Roommate Same Unit",
            "Other Cause", "Late Payments",
            "Lead Remediation", "Development",
            "Good Samaritan Ends"]
)

# Reshape the DataFrame to long format to put the eviction types in one column
evictions = pd.melt(evictions, id_vars=["File Date"], value_vars=["Non Payment", "Breach", "Nuisance",
                                                                  "Illegal Use", "Failure to Sign Renewal",
                                                                  "Access Denial", "Unapproved Subtenant",
                                                                  "Owner Move In", "Demolition",
                                                                  "Capital Improvement",
                                                                  "Substantial Rehab", "Ellis Act WithDrawal",
                                                                  "Condo Conversion", "Roommate Same Unit",
                                                                  "Other Cause", "Late Payments",
                                                                  "Lead Remediation", "Development",
                                                                  "Good Samaritan Ends"],
                    var_name="Eviction Type", value_name="Is True")

# Filter the rows where the condition is met
evictions = evictions[evictions["Is True"]]

evictions = evictions[evictions["Is True"]].copy()
evictions.drop("Is True", axis=1, inplace=True)

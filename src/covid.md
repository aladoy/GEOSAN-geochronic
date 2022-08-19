# Syndemic-project : Analysis framework of COVID-19 data

## Data Sources

| Variable                     | Source   | Timeframe           | N           | Covariates               | Resolution  | Scale    | Suggested outcome            | Outcome scale    |
| ---------------------------- | -------- | ------------------- | ----------- | ------------------------ | ----------- | -------- | ---------------------------- | ---------------- |
| **COVID-19 confirmed cases** | **CHUV** | **Mar 20 - Apr 22** | **35,876**  | **Age, Sex, Viral load** | **Address** | **Vaud** | **Percent positivity rate**  | **Populated ha** |
| **COVID-19 tests**           | **CHUV** | **Jan 20 - Apr 22** | **205,899** | **Age, Sex, Viral load** | **Address** | **Vaud** |                              | **Populated ha** |
| _COVID-19 hospitalizations_  | _OMC_    | _Jan 20 - Jun 21_   | _7,345_     |                          | _Zip code_  | _Vaud_   | _Hospitalization rates_      | _Zip code_       |
| _COVID-19 ICU admissions_    | _OMC_    | _Mar 20 - Jun 21_   | _1,343_     |                          | _Zip code_  | _Vaud_   | _Emergency admissions rates_ | _Zip code_       |
| COVID-19 deaths              | OMC      | Mar 20 - Jun 21     | 1,242       |                          | Zip code    | Vaud     |                              |                  |
| COVID-19 cases               | OFS      |                     |             | Age groups (10 yrs.)     | State       | CH       |                              |                  |
| COVID-19 tests               | OFS      |                     |             | Age groups (10 yrs.)     | State       | CH       |                              |                  |
| COVID-19 deaths              | OFS      |                     |             | Age groups (10 yrs.)     | State       | CH       |                              |                  |
| COVID-19 hospitalizations    | OFS      |                     |             | Age groups (10 yrs.)     | State       | CH       |                              |                  |
| COVID-19 vaccinated persons  | OFS      |                     |             | Age groups (10 yrs.)     | State       | CH       |                              |                  |
| KOF Stringency Index         | ETHZ     | Jan 20 -            |             |                          | State       | CH       |                              |                  |

## Steps

- Estimate the representivity of the CHUV data (compare to OFS data)
- Estimate COVID-19 stages for analysis (similar to Embury et al., 2022)

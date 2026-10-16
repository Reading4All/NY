# Cleaned DATA file: slfs_essa_financial_merged_thru2025.csv  
  
## SOURCE DATA, R script to Create the Clean DATA file:  
- raw data files: "slfs_financial_data.xlsx" (2024 on), "essa-financial-data.xlsx" (2023 and prior)  
- raw data files from: https://www.nysed.gov/fiscal-analysis-research/essa-and-slfs-financial-reporting  
- R Script to merge/clean them: slfs_essa_financial_merge.R  
 
 
# Cleaned DATA file: ST3-SPED-202X.csv  (one per year)  
  
## SOURCE DATA, R script to Create the Clean DATA file:  
- raw data files: 2024-2025_School_Year_2025-2026_SAMS ST-3.xlsx  (one per year)  
- raw data files from: https://stateaid.nysed.gov/st3/st3data.htm  
- R Script to merge/clean them: Clean_ST3-SPED.R  
 
 
# Cleaned DATA file: personnel-master-thru2025.csv  
  
## SOURCE DATA, R script to Create the Clean DATA file:  
- raw data files: personnel-master-file-statistical-run-202X-2X.xlsx  (one per year)  
- raw data files from: https://www.nysed.gov/information-reporting-services/personnel-master-file-pmf  
- R Script to merge/clean them: personnel-master-Clean_Merge.R  
  
  
# OTHER - Teacher info:  
## ST3 info: Classroom Teacher	Teacher Salaries	Mainly the .15 codes, e.g. A2110.11 to A2110.14 and A2250.15
- Guidance, Nurses, Psychologists, Social Workers	Instructional Salaries for Pupil Personnel Services	A2805.15 to A2830.15
- Principals, Program Administration	Curriculum Development and Supervision (my best match, see below)	A2010.15, A2020.15, A2040.15
- Central Administration	Central Administration	A1240, A1310, A1345, A1430, A1460, A1480, and others
- Other Non-Teaching Staff	Other Instructional Salaries	.15 and .16 codes (pre-K teachers, librarians, paraprofessionals, secretaries)


# OTHER - GENERAL SPENDING NOTES:  
## ESSA/SLFS (School-Level Finance Survey) Special Ed Spending info:
- It reflects direct classroom instruction (A2250 operating lines and F2250 school-age IDEA Part B grant lines).
- It excludes four major special education spending areas:
    Out-of-District Tuition & Placements (Categorized under "Exclusions"):
        A2250.471: Public school district tuition
        A2250.472: Approved private 853 school and Special Act district tuition
        A2250.473: Direct special education payments to charter schools
        Why excluded: These tuition checks are stripped out of school building totals so they do not artificially distort individual school per-pupil averages.
    Special Education Transportation (Categorized under "Transportation"):
        General Fund Busing: Special education bus drivers/monitors (A5510.160/.161), contracted vendors (A5510.400), and BOCES busing (A5581.49)
        Summer ESY Busing: Section 4408 July/August special education transit (F5511, F5541, F5582)
    Related Services & Central Administration (Categorized under "Instructional Support" or "Central Overhead"):
        School Psychologists (A2820): Evaluators, test protocols, and certified psychologist salaries
        School Social Workers (A2825): Certified social workers and counseling services
        Pupil Personnel Services Directors (A2830): Special education administrators and central PPS office staff
        Special Education Legal Counsel (A1420.400): Retainers, attorneys, and impartial hearing costs (charged to central board legal)
    Summer Extended School Year Programs (Categorized under "Summer Programs"):
        F2253: July/August Section 4408 summer special education instructional costs
- It also excludes:
    School legal spending
- The website has a spreadsheet to show each ST3 code that's included as SLSF "Special Ed"

## ST-3:
- It relects all spending (SPED (from A(gen ed) and F(special funding) and SPED exclusions, and non SPED spending too
- It is the root source of all finanical data - the ESS/SLFS spreadsheet, MOE, etc (which each excluding different things in their Special Ed totals and exclusions).


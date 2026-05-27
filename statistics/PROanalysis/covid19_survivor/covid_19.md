https://data.mendeley.com/datasets/kfk5j9g4xc/1

Steps to reproduce
Data were collected using an anonymous online questionnaire that was designed following Helsinki’s declaration of ethics. Participation was voluntary and the access to the questionnaire was only given to participant who provided his/her consent by checking a box in the online questionnaire. Eligible patients were aged 18 years old and above, who had a positive SARS-CoV-2 infection confirmed by PCR test, were admitted to hospital and were discharged for more than 30 days and not more than 90 days. 
The online survey includes: (i) socio-demographic and clinical characteristics (age, gender, marital status, educational level, employment status, place of residence, socio-economic level, presence of chronic diseases specifically type 1 and 2 diabetes, hypertension, kidney and cardiovascular diseases and ICU admission (yes/no)) and (ii) health related quality of life using the EQ-5D-5L instrument, which consists of two sections: the descriptive system and a Visual Analog Scale (VAS). The descriptive system comprises five health dimensions, including mobility, self-care, usual activities, pain/ discomfort and anxiety/ depression. For each dimension there are 5 levels (5L) to represent the degree of the health state severity. The Visual Analog Scale (VAS) allows the individual to appreciate his/her current health states (scale 0–100, where 0=the worse imaginable and 100=the best imaginable).

```{r}

dplyr::rename(covid19_dat, 
id = "N° participant", 
gender = "Q1", 
age_cat = "Q2", 
married = "Q3", 
education_cat = "Q4", 
employment = "Q5", 
address = "Q6", 
SES = "Q7", 
t1dm = "Q8", 
t2dm = "Q9",
htn = "Q10", 
ckd = "Q11", 
cvd = "Q12", 
icu = "Q13", 
eq5d5l_mobility = "Q14", 
eq5d5l_selfcare = "Q15", 
eq5d5l_activity = "Q16", 
eq5d5l_pain = "Q17", 
eq5d5l_anxiety = "Q18", 
eq5d5l_vas = "Q19"
)

```
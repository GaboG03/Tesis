# Librerías: 

library(readr)
library(dplyr)
library(ggplot2)

# Datos: 

insurance_claims <- read_csv("datos/insurance_claims.csv")

insurance_claims %>% nrow()
insurance_claims %>% ncol()

summary(insurance_claims)

## months_as_customer: Variable numérica, que indica Número de meses como cliente. 
## age: Variable numérica,  edad del cliente.
## policy_number: Número de poliza. 
## policy_bind_date: Fecha de vinculación de la poliza. 
## policy_state: Estado de la poliza, toma los valores IL, IN, OH.  
## policy_csl: Límites de responsabilidad civil de una póliza de seguro de automóvil, toma los valores : 100/300, 250/500, 500/1000. 
# "100/300" indica un límite de $100,000 para lesiones corporales por persona y $300,000 en total por accidente.
## policy_deductable: Variable numérica, corresponde a la cantidad de dinero que el asegurado debe pagar de su propio bolsillo antes de que la compañía de seguros comience a cubrir los costos de un reclamo.
## policy_annual_premium: Variable numérica, prima anual de la póliza de seguro. La prima es el monto de dinero que el asegurado paga a la compañía de seguros a cambio de la cobertura proporcionada por la póliza durante un período específico, que generalmente es de un año.
## umbrella_limit: Variable numérica, límite de la poliza de seguro de responsabilidad civil adicional que proporciona una protección extra de los límites estándar de la póliza. 
## insured_zip: Código Postal del asegurado. 
## insured_sex: Sexo del asegurado, toma los valores FEMALE, MALE. 

table(insurance_claims$insured_sex)

## insured_education_level: Variable categórica, nivel de educación del asegurado: Associate, College High School, JD, Masters, MD, PhD.

table(insurance_claims$insured_education_level)

## insured_occupation: Ocupación del asegurado, 14 valores distintos: adm-clerical, armed-forces, craft-repair.

table(insurance_claims$insured_occupation)

## insured_hobbies: Pasatiempos del asegurado: base-jumping, basketball, board-game. 

table(insurance_claims$insured_hobbies)

## insured_relationship: Estado civil del asegurado, toma los valores:  husband,  not-in-family other-relative, own-child, unmarried, wife.


table(insurance_claims$insured_relationship)

## capital-gains: Ganancias del capital. 

## capital-loss: Pérdidas del capital. 


## Variables relativas al incidente: 

# incident_date: Fecha en que ocurrió el incidente. 

## incident_type: Variable categórica que indica el tipo de incidente ocurrido, toma los valores: Multi-vehicle Collision, Parked Car Single Vehicle Collision, Vehicle Theft.  

table(insurance_claims$incident_type)

## collision_type: Tipo de colisión que ocurrió en el accidente, toma los valores de  ?, Front Collision , Rear Collision, Side Collision. 

table(insurance_claims$collision_type)

## incident_severity: Nivel de gravedad del incidente:  Major Damage, Minor Damage, Total Loss, Trivial Damage. 

table(insurance_claims$incident_severity)

## authorities_contacted: Tipo de autoridad contactada. 

table(insurance_claims$authorities_contacted)

## incident_state: Estado donde ocurrió el accidente: NC  NY  OH  PA  SC  VA  WV 

table(insurance_claims$incident_state)

## incident_city: Ciudad donde ocurrió el incidente: Arlington, Columbus,   Hillsdale,   Northbend,  Northbrook,   Riverwood, Springfield.   

table(insurance_claims$incident_city)

## incident_location: Ubicación donde ocurrió el incidente o el accidente, se tienen 1000 valores distintos.

table(insurance_claims$incident_location)


## incident_hour_of_the_day: Variable numérica discreta que indica la hora del día en que ocurrió el incidente o el accidente. 

## number_of_vehicles_involved: Variable numérica discreta que indica el número de vehículos involucrados en el incidente o el accidente. 

## property_damage: Indica si existen daños o no a la propiedad externa como resultado del incidente o accidente.

table(insurance_claims$property_damage)

## bodily_injuries: Número total de lesiones reportadas como resultado del incidente o accidente de tránsito. 

## witnesses: Indica el número total de testigos presentes en el lugar del incidente o accidente.   

## police_report_available: Variable categórica que indica si existe o no un informe policial sobre el incidente o accidente.

summary(insurance_claims)

table(insurance_claims$police_report_available)


## total_claim_amount: Cantidad total reclamada por el asegurador como resultado del incidente o accidente.
## injury_claim: Monto reclamado por lesiones como resultado del incidente o accidente
## property_claim: Monto reclamado por daños a la propiedad como resultado de un accidente 
## vehicle_claim: Monto reclamado específicamente por los daños al vehículo asegurado como resultado de un accident     

## auto_make: Marca del Auto: 14 marcas.
table(insurance_claims$auto_make)
##auto_model: Modelo del auto: 39 modelos distintos. 

table(insurance_claims$auto_model)

# auto_year: Anio del vehículo.    

# fraud_reported: Variable binaria que indica si se ha reportado o no un fraude en relación con un reclamo de seguro de automóvil. 

table(insurance_claims$fraud_reported)/insurance_claims %>% nrow()

insurance_claims<- insurance_claims %>% dplyr::select(-policy_number, -insured_zip, -incident_location,
                                                      -auto_model)




ggplot(insurance_claims, aes(x = fraud_reported, fill = fraud_reported)) +
  geom_bar() +
  stat_count(geom = "text",
             aes(label = after_stat(count)), 
             vjust = -.5)+
  labs(title = "Distribución de la variable Fraud Reported", x = "Fraud Reported", y = "Frecuencia") +
  theme_light()

# theme_classic


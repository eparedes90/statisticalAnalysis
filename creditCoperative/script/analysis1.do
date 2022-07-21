
*reemplazar por la dirección donde están tu carpeta"
cd "E:\Asesorías de tesis\Economía\Asociatividad y servicios financieros\"
clear 

*reemplazar por el lugar del archivo 
import delimited "forAnalysis\data2.csv"

*cambiar NA por missing
foreach var of varlist * {
cap replace `var' = "" if `var'=="NA"
}




*encode
encode p902, generate(credito)
encode p801, generate(asociacion)
encode p1105, generate(educacion)

list asociacion in 50/100, nolab
list educacion in 50/100, nolab
describe asociacion

*regresión lineal
regress credito propietario asociacion educacion

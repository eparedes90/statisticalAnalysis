




cd "E:\Asesorías de tesis\Economía\Asociatividad y servicios financieros\"
use 16_Cap800.dta

* FILTRAR SOLO A LAS PERSONAS NATURALES
drop if ESTRATO == 1
drop if ESTRATO == 3
drop if ESTRATO == 4

* SOLO A PEQUEÑOS Y MEDIANOS PRODUCTORES
drop if CODIGO == 2

* SOLO ACTIVIDAD AGRÍCOLA
drop if P102_1 == 0
drop if RESFIN == 2
table CCDI
table NSELUA

*CREAR CODIGO PARA CADA UNIDAD AGROPECUARIA
gen codigoUnico = CCDD + CCPP + CCDI + CONGLOMERADO + NSELUA + UA
by codigoUnico, sort: gen nvals = _n == 1
count if nvals


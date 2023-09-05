clear
graph drop _all
set more off
program drop _all

global directory "C:\Users\Utente\Desktop\BUSINESS ANALYTICS\progetto_dati\"
cd `"$directory"'

//cap log close
//log using "sangue.log", t replace

//insheet using "FINAL_DATASET.csv", names clear
import delimited "FINAL_DATASET.csv", varnames(1) rowrange(5) bindquote(strict) clear 
destring , replace
//import excel "C:\Users\micsc\Desktop\Warp Zone\Code\STATA\sangue\data.xlsx", sheet("data") firstrow case(lower) clear mike 

//età, niente minorenni
rename q10 eta
drop if eta < 18

drop ipaddress

//age cohort
label define L_etacohort 0 "18-25" 1 "26-35" 2 "36-45" 3 "46-55" 4 "50 in poi" //source: AVIS website
gen eta_cohort = 0
replace eta_cohort = 1 if eta >= 26 & eta <= 33
replace eta_cohort = 2 if eta >= 34 & eta <= 41
replace eta_cohort = 3 if eta >= 42 & eta <= 49
replace eta_cohort = 4 if eta >= 50
label values eta_cohort L_etacohort

//giovane
//label define L_giovane 0 "Non Giovane" 1 "Giovane"
//gen giovane = eta_cohort == 0
//label values giovane L_giovane

//materie infermieristiche e simili 
label define L_matinf 0 "No" 1 "Si"
rename q23 matinf
label values matinf L_matinf

//dimensione città
label define L_dimensionecitta 1 "Piccola (<50k)" 2 "Media (<250k)" 3 "Grande (>250k)"
rename q20 dimensionecitta
label values dimensionecitta L_dimensionecitta

//parenti o amici
label define L_parentioamici 0 "No" 1 "Si"
rename q26 parentioamici
label values parentioamici L_parentioamici

//campagne sensibilizzazione
label define L_campagne 0 "No / Non so" 1 "Si"
rename q27 campagne
replace campagne = 0 if campagne == -1
label values campagne L_campagne

//motivo non dono
label define L_motivonondono 1 "Salute" 2 "PauraAgo" 3 "PensoFacciaMale" 4 "AndareDaSolo" 5 "Tempo" 6 "NonPensato" 7 "NonMiInteressa" 8 "Altro" 11 "AltroInconciliabile"
rename q2 motivonondono
label values motivonondono L_motivonondono

//genere
label define L_genere 1 "Uomo" 2 "Donna" 3 "Altro"
rename q19 genere
label values genere L_genere

//donatore
label define L_donatore 0 "Non Donatore" 1 "Donatore"
rename qid1 donatore
label values donatore L_donatore

//contogenere
capture drop contogenere
egen contogenere = count(genere), by(genere)

//IPOTESI 1

gen paura = 0
replace paura = 1 if ((motivonondono == "PauraAgo":L_motivonondono) | (motivonondono == "PensoFacciaMale":L_motivonondono) | (motivonondono == "AndareDaSolo":L_motivonondono))
replace paura = . if (motivonondono == .)

preserve

drop if donatore == 1
drop if motivonondono == "Salute":L_motivonondono | motivonondono == "AltroInconciliabile":L_motivonondono
drop if genere == "Altro":L_genere

//consideriamo solo i giovani paurosi
drop if eta_cohort >= 3

reg paura i.genere i.eta_cohort i.matinf, robust 

//contogenere
capture drop contogenere
egen contogenere = count(genere), by(genere)

//tabstat paura, by(genere)
//tabulate genere, summarize(paura)
//tabulate paura genere, chi2 expected
//tabulate paura eta_cohort, chi2 expected

ttest paura == 0.33
//ttest paura, by(giovane)
//ttest paura, by(genere)

reg paura
//usare i pweight può essere pericoloso, i.e. se vuoi controllare per età devi usare contogenereperagecohort
//non serve controllare per cohort perchè la maggior parte del sample è giovane (18-25), e paura è bilanciato (50%) nei giovani
//comunque, fingendo che il sample abbia 50/50 uomini e donne il risultato non cambia particolarmente
reg paura [pweight = _N/contogenere]

//non c'è particolare differenza fino al cohort 34-41, poi i più vecchi da lì in poi sono nettamente meno paurosi
reg paura i.genere, robust //i.eta_cohort  i.parentioamici i.matinf

local val = 0.33
//solo uomini
test (_b[_cons]) = `val'

local sign_ = sign((_b[_cons])-`val')

display "Ho: coef <= `val'  p-value = " ttail(r(df_r),`sign_'*sqrt(r(F)))

//50 50 uomini e donne
test (_b[_cons] + 0.5 * _b[2.genere]) = `val' //ipotizziamo che nella popolazione ci siano 50% donne e 50% uomini 

local sign_ = sign((_b[_cons] + 0.5 * _b[2.genere])-`val')

display "Ho: coef <= `val'  p-value = " ttail(r(df_r),`sign_'*sqrt(r(F)))

logit paura i.genere i.eta_cohort, robust
margins, dydx(*)

restore


//Q51 (control)
//Saresti disposto/propenso a donare in futuro?
gen propensoadonare = .
replace propensoadonare = 1 if (donatore == 0 & (q51 == 1 & q51 < .))
replace propensoadonare = 0 if (donatore == 0 & (q51 != 1 & q51 < .))

tab propensoadonare eta_cohort, column

//Q45
//Cosa pensi che ti aiuterebbe o renderebbe più propenso alla donazione?
gen q45_coetaneovolontario = 0
replace q45_coetaneovolontario = 1 if strpos(q45, "1") | strpos(q45, "3") 
replace q45_coetaneovolontario = . if q45 == ""

preserve
drop if donatore == 1
drop if q45 == ""
drop if paura == 0


//drop if eta_cohort >= 3

reg q45_coetaneovolontario propensoadonare i.genere i.matinf i.eta_cohort i.dimensionecitta, robust 

reg q45_coetaneovolontario
local val = 0.5
//propensoadonare
test (_b[_cons]) = `val'

local sign_ = sign((_b[_cons])-`val')

display "Ho: coef <= `val'  p-value = " ttail(r(df_r),`sign_'*sqrt(r(F))) //questo non è stat sign 

local val = 0.66
//propensoadonare
test (_b[_cons]) = `val'

local sign_ = sign((_b[_cons])-`val')

display "Ho: coef <= `val'  p-value = " ttail(r(df_r),`sign_'*sqrt(r(F)))

ttest q45_coetaneovolontario == 0.5
ttest q45_coetaneovolontario == 0.60
ttest q45_coetaneovolontario == 0.66
ttest q45_coetaneovolontario == 0.66 if eta_cohort < 3
ttest q45_coetaneovolontario == 0.5 if propensoadonare == 1
ttest q45_coetaneovolontario == 0.5 if propensoadonare == 0

//ttest q45_coetaneovolontario, by(propensoadonare)
restore


//Q46
//Pensi che ti aiuterebbe o saresti più propenso alla donazione se potessi ricevere supporto da un donatore abituale il giorno della donazione?
gen q46_si_esupporto = 0
replace q46_si_esupporto = 1 if q46 > 0
replace q46_si_esupporto = . if q46 == .

preserve
drop if donatore == 1
drop if paura == 0
drop if q46 == .

//drop if eta_cohort >= 3

local variabile = `"q46_si_esupporto"'

reg `variabile' propensoadonare i.genere i.matinf i.eta_cohort i.parentioamici i.dimensionecitta, robust

reg `variabile' 
local val = 0.5

test (_b[_cons]) = `val'

local sign_ = sign((_b[_cons])-`val')

display "Ho: coef <= `val'  p-value = " ttail(r(df_r),`sign_'*sqrt(r(F)))

local val = 0.66

test (_b[_cons]) = `val'

local sign_ = sign((_b[_cons])-`val')

display "Ho: coef <= `val'  p-value = " ttail(r(df_r),`sign_'*sqrt(r(F)))

ttest `variabile' == 0.5
ttest `variabile' == 0.66
ttest `variabile' == 0.5 if propensoadonare == 1
ttest `variabile' == 0.5 if propensoadonare == 0
restore


//Q47
//Pensi che ti aiuterebbe o saresti più propenso alla donazione se potessi andare a donare insieme a un donatore abituale?
gen q47_si_esupporto = 0
replace q47_si_esupporto = 1 if q47 > 0
replace q47_si_esupporto = . if q47 == .

preserve
drop if donatore == 1
drop if paura == 0
drop if q47 == .

//drop if eta_cohort >= 3

local variabile = `"q47_si_esupporto"'

reg `variabile' propensoadonare i.genere i.matinf i.eta_cohort i.parentioamici i.dimensionecitta, robust
reg `variabile' 
local val = 0.5

test (_b[_cons]) = `val'

local sign_ = sign((_b[_cons])-`val')

display "Ho: coef <= `val'  p-value = " ttail(r(df_r),`sign_'*sqrt(r(F)))

local val = 0.66

test (_b[_cons]) = `val'

local sign_ = sign((_b[_cons])-`val')

display "Ho: coef <= `val'  p-value = " ttail(r(df_r),`sign_'*sqrt(r(F)))


ttest `variabile' == 0.5
ttest `variabile' == 0.66
ttest `variabile' == 0.5 if propensoadonare == 1
ttest `variabile' == 0.5 if propensoadonare == 0
restore


//Q49
//Hai mai cercato online un contatto diretto con dei donatori o un servizio simile a quello appena descritto?
gen q49_si_esupporto = 0
replace q49_si_esupporto = 1 if q49 > 0
replace q49_si_esupporto = . if q49 == .

preserve
drop if donatore == 1
drop if paura == 0
drop if q49 == .

//drop if eta_cohort >= 3

local variabile = `"q49_si_esupporto"'

reg `variabile' propensoadonare i.genere i.matinf i.eta_cohort i.parentioamici i.dimensionecitta, robust

reg `variabile'
local val = 0.5
test (_b[_cons]) = `val'

local sign_ = sign((_b[_cons])-`val')

display "Ho: coef <= `val'  p-value = " ttail(r(df_r),`sign_'*sqrt(r(F)))

local val = 0.66
test (_b[_cons]) = `val'

local sign_ = sign((_b[_cons])-`val')

display "Ho: coef <= `val'  p-value = " ttail(r(df_r),`sign_'*sqrt(r(F)))

ttest `variabile' == 0.5
ttest `variabile' == 0.66
ttest `variabile' == 0.5 if propensoadonare == 1
ttest `variabile' == 0.5 if propensoadonare == 0
restore

//Q48, per le persone che hanno detto di sì a q45,46,47
gen propenso_aiuto = 0
replace propenso_aiuto = 1 if (q45_coetaneovolontario == 1) | (q46_si_esupporto == 1) | (q47_si_esupporto == 1)
replace propenso_aiuto = . if q45 == "" | q46 == . | q47 == .
preserve
drop if donatore == 1
drop if paura == 0
drop if q48 ==""

//drop if eta_cohort >= 3

tab propenso_aiuto

//drop if propenso_aiuto == 0

gen q48_app = 0
replace q48_app = 1 if (strpos(q48, "1") | strpos(q48, "2") | strpos(q48, "3"))
replace q48_app = . if q48 == ""

reg q48_app propensoadonare propenso_aiuto i.genere i.matinf i.eta_cohort i.parentioamici i.dimensionecitta, robust
reg q48_app propensoadonare i.genere i.matinf i.eta_cohort i.parentioamici i.dimensionecitta if propenso_aiuto == 1, robust //non conta propenso a donare 
reg q48_app propensoadonare propenso_aiuto
reg q48_app propensoadonare propenso_aiuto if propenso_aiuto ==1 
reg q48_app if propenso_aiuto ==1 
local val = 0.50 

test (_b[_cons]) = `val'

local sign_ = sign((_b[_cons])-`val')

display "Ho: coef <= `val'  p-value = " ttail(r(df_r),`sign_'*sqrt(r(F)))

local val = 0.66 

test (_b[_cons]) = `val'

local sign_ = sign((_b[_cons])-`val')

display "Ho: coef <= `val'  p-value = " ttail(r(df_r),`sign_'*sqrt(r(F)))

ttest q48_app == 0.5 
ttest q48_app == 0.66
ttest q48_app == 0.5 if propenso_aiuto == 1
ttest q48_app == 0.66 if propenso_aiuto == 1

ttest q48_app == 0.5 if propenso_aiuto == 1 & propensoadonare == 1
ttest q48_app == 0.5 if propenso_aiuto == 1 & propensoadonare == 0


restore

//Q57
//Questa piattaforma esiste. Ti mette in contatto con i donatori, chiarisce dubbi, condivide eventi organizzati dalle associazioni (es. Avis, Croce Rossa), trova il centro donazioni più vicino a te. Ti iscriveresti? 
preserve
drop if donatore == 1
drop if paura == 0
drop if q57 == .

//consideriamo solo i giovani, più paurosi
//drop if eta_cohort >= 3

tab q57

reg q57 propensoadonare propenso_aiuto i.genere i.matinf i.eta_cohort i.parentioamici i.dimensionecitta, robust
reg q57 
local val = 0.50 

test (_b[_cons]) = `val'

local sign_ = sign((_b[_cons])-`val')

display "Ho: coef <= `val'  p-value = " ttail(r(df_r),`sign_'*sqrt(r(F)))

local val = 0.66 

test (_b[_cons]) = `val'

local sign_ = sign((_b[_cons])-`val')

display "Ho: coef <= `val'  p-value = " ttail(r(df_r),`sign_'*sqrt(r(F)))

ttest q57 == 0.5 if propenso_aiuto == 1
ttest q57 == 0.66 if propenso_aiuto == 1
ttest q57 == 0.5 if propenso_aiuto == 1 & propensoadonare == 1
ttest q57 == 0.5 if propenso_aiuto == 1 & propensoadonare == 0

restore


//Q77, "DiD"
//sample troppo piccolo per farci niente
gen malinformato = .
replace malinformato = 0 if q76 == 3 & q76 < .
replace malinformato = 1 if q76 != 3 & q76 < .

gen malinformato_sovrastimato = .
replace malinformato_sovrastimato = 0 if q76 <= 3 & q76 < .
replace malinformato_sovrastimato = 1 if q76 > 3 & q76 < .

tab propensoadonare q77, row

preserve

keep if malinformato_sovrastimato == 1
tab propensoadonare q77, row

restore


//DISINTERESSATI
gen disinteressato = 0
replace disinteressato = 1 if ((motivonondono == "Tempo":L_motivonondono) | (motivonondono == "NonPensato":L_motivonondono) | (motivonondono == "NonMiInteressa":L_motivonondono))
replace disinteressato = . if (motivonondono == .)

preserve

drop if donatore == 1
drop if motivonondono == "Salute":L_motivonondono | motivonondono == "AltroInconciliabile":L_motivonondono
drop if genere == "Altro":L_genere

//drop if eta_cohort >= 3

ttest disinteressato == 0.33
ttest disinteressato, by(genere)

reg disinteressato i.genere i.matinf i.eta_cohort i.parentioamici i.campagne, robust

reg disinteressato i.genere i.parentioamici, robust

local val = 0.33

test (_b[_cons]) = `val'

local sign_ = sign((_b[_cons])-`val')

display "Ho: coef <= `val'  p-value = " ttail(r(df_r),`sign_'*sqrt(r(F)))

//50 50 uomini e donne senza amici che donano
test (_b[_cons] + 0.5 * _b[2.genere]) = `val'

local sign_ = sign((_b[_cons] + 0.5 * _b[2.genere])-`val')

display "Ho: coef <= `val'  p-value = " ttail(r(df_r),`sign_'*sqrt(r(F)))

logit disinteressato i.genere i.parentioamici, robust
margins, dydx(*)
restore


//Q55
//Se sapessi che molti tuoi amici e conoscenti donano il sangue, saresti più propenso a informarti o a considerare di donare in futuro?
gen q55_si = .
replace q55_si = 1 if q55 == 1 & q55 < .
replace q55_si = 0 if q55 != 1 & q55 < .

preserve
drop if donatore == 1
drop if disinteressato == 0
drop if q55 == .

//consideriamo solo i giovani, più paurosi
//drop if eta_cohort >= 3

local variabile = `"q55_si"'

reg `variabile' i.parentioamici##propensoadonare i.genere i.matinf i.eta_cohort i.dimensionecitta, robust

reg `variabile' i.parentioamici 

ttest `variabile' == 0.5
ttest `variabile' == 0.66
ttest `variabile' == 0.5 if parentioamici == 1
ttest `variabile' == 0.5 if parentioamici == 0
ttest `variabile' == 0.66 if parentioamici == 1
ttest `variabile' == 0.66 if parentioamici == 0
ttest `variabile' == 0.5 if propensoadonare == 1
ttest `variabile' == 0.5 if propensoadonare == 0

//ttest `variabile' == 0.5 if propensoadonare == 1 & parentioamici == 1
//ttest `variabile' == 0.5 if propensoadonare == 0 & parentioamici == 1
//ttest `variabile' == 0.5 if propensoadonare == 1 & parentioamici == 0
//ttest `variabile' == 0.5 if propensoadonare == 0 & parentioamici == 0

restore

//Q59
//Se un tuo parente o amico ti invitasse a donare o a informarti sulla donazione del sangue, saresti più propenso a informarti ulteriormente o a considerare di donare in futuro? 
gen q59_si = .
replace q59_si = 1 if q59 == 1 & q59 < .
replace q59_si = 0 if q59 != 1 & q59 < .

preserve
drop if donatore == 1
drop if disinteressato == 0
drop if q59 == .

//drop if eta_cohort >= 3

local variabile = `"q59_si"'

reg `variabile' propensoadonare i.parentioamici i.genere i.matinf i.eta_cohort i.dimensionecitta, robust

reg `variabile' 
local val = 0.5

test (_b[_cons]) = `val'

local sign_ = sign((_b[_cons])-`val')

display "Ho: coef <= `val'  p-value = " ttail(r(df_r),`sign_'*sqrt(r(F)))

local val = 0.66

test (_b[_cons]) = `val'

local sign_ = sign((_b[_cons])-`val')

display "Ho: coef <= `val'  p-value = " ttail(r(df_r),`sign_'*sqrt(r(F)))


ttest `variabile' == 0.5
ttest `variabile' == 0.66
ttest `variabile' == 0.5 if propensoadonare == 1
ttest `variabile' == 0.5 if propensoadonare == 0

restore

//Q60
//Se un tuo parente o amico ti invitasse su un'applicazione che ti mette in contatto con i donatori, ti aiuta a chiarire dubbi, mostra eventi organizzati dalle associazioni come Avis o Croce Rossa e trova il centro donazioni più vicino a te, pensi che ti  iscriveresti?
gen q60_si = .
replace q60_si = 1 if q60 == 1 & q60 < .  
replace q60_si = 0 if q60 != 1 & q60 < . 

preserve
drop if donatore == 1
drop if disinteressato == 0
drop if q60 == .


//drop if eta_cohort >= 3

local variabile = `"q60_si"'

reg `variabile' i.parentioamici##propensoadonare i.genere i.matinf i.eta_cohort i.dimensionecitta, robust

reg `variabile' i.parentioamici


ttest `variabile' == 0.5
ttest `variabile' == 0.66
ttest `variabile' == 0.66 if parentioamici == 1
ttest `variabile' == 0.66 if parentioamici == 0

ttest `variabile' == 0.5 if propensoadonare == 1 
ttest `variabile' == 0.66 if propensoadonare == 1 
ttest `variabile' == 0.5 if propensoadonare == 0
ttest `variabile' == 0.5 if propensoadonare == 1 & parentioamici == 1
ttest `variabile' == 0.5 if propensoadonare == 0 & parentioamici == 1
ttest `variabile' == 0.5 if propensoadonare == 1 & parentioamici == 0
ttest `variabile' == 0.5 if propensoadonare == 0 & parentioamici == 0

restore

//DONATORI
rename q79 volontario
//Q29
//Cosa ti ha spinto a cominciare a donare il sangue?
gen q29_amico = .
replace q29_amico = 1 if q29 == 1 & q29 < .
replace q29_amico = 0 if q29 != 1 & q29 < .

preserve
drop if donatore == 0 
drop if q29 == .

reg q29_amico i.genere i.matinf i.eta_cohort i.dimensionecitta

reg q29_amico

local val = 0.33
test (_b[_cons]) = `val'

local sign_ = sign((_b[_cons])-`val')

display "Ho: coef <= `val'  p-value = " ttail(r(df_r),`sign_'*sqrt(r(F)))

ttest q29_amico = 0.33
restore
//Q37
//Hai mai invitato alcuni dei tuoi amici o conoscenti a donare?

preserve 
drop if donatore == 0 
drop if q37 == .

reg q37 i.genere i.matinf i.eta_cohort i.dimensionecitta, robust
reg q37
local val = 0.5

test (_b[_cons]) = `val'

local sign_ = sign((_b[_cons])-`val')

display "Ho: coef <= `val'  p-value = " ttail(r(df_r),`sign_'*sqrt(r(F)))

local val = 0.66

test (_b[_cons]) = `val'

local sign_ = sign((_b[_cons])-`val')

display "Ho: coef <= `val'  p-value = " ttail(r(df_r),`sign_'*sqrt(r(F)))

ttest q37 == 0.5
ttest q37 == 0.66
restore 

//Q31
//Ci sono persone che non donano perché impaurite, dubbiose o disinformate. Dedicheresti del tempo a:

gen q31_agire = .
replace q31_agire = 0 if donatore == 1
replace q31_agire = 1 if donatore == 1 & (strpos(q31, "1") | strpos(q31, "2") | strpos(q31, "3"))

replace q31_agire = 0 if donatore == 1 & (strpos(q31, "4") | q31 == "")

summarize q31_agire

preserve 
drop if donatore == 0
drop if q31 == ""

reg q31_agire volontario i.genere i.matinf i.eta_cohort i.dimensionecitta, robust

reg q31_agire
local val = 0.5

test (_b[_cons]) = `val'

local sign_ = sign((_b[_cons])-`val')

display "Ho: coef <= `val'  p-value = " ttail(r(df_r),`sign_'*sqrt(r(F)))

local val = 0.66

test (_b[_cons]) = `val'

local sign_ = sign((_b[_cons])-`val')

display "Ho: coef <= `val'  p-value = " ttail(r(df_r),`sign_'*sqrt(r(F)))

ttest q31_agire == 0.5
ttest q31_agire == 0.66
restore 

//Q35

preserve
drop if donatore == 0 
drop if q35 == .
reg q35 volontario i.genere i.matinf i.eta_cohort i.dimensionecitta, robust  
reg q35
local val = 0.5

test (_b[_cons]) = `val'

local sign_ = sign((_b[_cons])-`val')

display "Ho: coef <= `val'  p-value = " ttail(r(df_r),`sign_'*sqrt(r(F)))

local val = 0.66

test (_b[_cons]) = `val'

local sign_ = sign((_b[_cons])-`val')

display "Ho: coef <= `val'  p-value = " ttail(r(df_r),`sign_'*sqrt(r(F)))

ttest q35 == 0.5
ttest q35 == 0.66
ttest q35 == 0.75


restore 
//Q67
gen q67_usare = .
replace q67_usare = 0 if donatore == 1
replace q67_usare = 1 if donatore == 1 & (strpos(q67, "1") | strpos(q67, "2") | strpos(q67, "3") | strpos(q67, "4") )

replace q67_usare = 0 if donatore == 1 & (strpos(q67, "5") | strpos(q67, "6") | q67 == "")

preserve 
drop if donatore == 0
drop if q67 == ""
summarize q67_usare

reg q67_usare volontario i.genere i.matinf i.eta_cohort i.dimensionecitta, robust
reg q67_usare 

ttest q67_usare == 0.5
ttest q67_usare == 0.66
local val = 0.5

test (_b[_cons]) = `val'

local sign_ = sign((_b[_cons])-`val')

display "Ho: coef <= `val'  p-value = " ttail(r(df_r),`sign_'*sqrt(r(F)))

local val = 0.66

test (_b[_cons]) = `val'

local sign_ = sign((_b[_cons])-`val')

display "Ho: coef <= `val'  p-value = " ttail(r(df_r),`sign_'*sqrt(r(F)))
restore

//Q69
preserve 
drop if donatore == 0 
drop if q69 ==.
reg q69 volontario i.genere i.matinf i.eta_cohort i.dimensionecitta, robust
reg q69
local val = 0.5

test (_b[_cons]) = `val'

local sign_ = sign((_b[_cons])-`val')

display "Ho: coef <= `val'  p-value = " ttail(r(df_r),`sign_'*sqrt(r(F)))

local val = 0.66

test (_b[_cons]) = `val'

local sign_ = sign((_b[_cons])-`val')

display "Ho: coef <= `val'  p-value = " ttail(r(df_r),`sign_'*sqrt(r(F)))

ttest q69 == 0.5
ttest q69 == 0.66

restore 
//Q30

preserve 
drop if q30 == . 
reg q30 donatore i.genere i.matinf i.eta_cohort i.dimensionecitta, robust
logit q30 donatore i.genere i.matinf i.eta_cohort, robust
margins, dydx(*)
reg q30 propensoadonare i.genere i.matinf i.eta_cohort i.dimensionecitta if (donatore == 0), robust 

reg q30 paura disinteressato i.propensoadonare i.genere i.matinf i.eta_cohort i.dimensionecitta if (donatore == 0), robust 

summarize q30


ttest q30 == 0.5
ttest q30 == 0.66
ttest q30 == 0.5 if (donatore == 1)
ttest q30 == 0.5 if (donatore == 0)
ttest q30 == 0.66 if (donatore == 1)
ttest q30 == 0.66 if (donatore == 0)
ttest q30 == 0.5 if (donatore == 0 & propensoadonare == 1)
ttest q30 == 0.5 if (donatore == 0 & propensoadonare == 0)
ttest q30 = 0.33 if q57==0| q60_si == 0
restore
//Q81
preserve 
drop if q81 == .
reg q81 donatore i.genere i.matinf i.eta_cohort i.dimensionecitta, robust
logit q81 donatore i.genere i.matinf i.eta_cohort, robust
margins, dydx(*)
reg q81 propensoadonare i.genere i.matinf i.eta_cohort i.dimensionecitta if (donatore == 0), robust 

reg q81 paura disinteressato i.propensoadonare  i.genere i.matinf i.eta_cohort i.dimensionecitta if (donatore == 0), robust 

summarize q81

ttest q81 == 0.5
ttest q81 == 0.5 if (donatore == 1)
ttest q81 == 0.5 if (donatore == 0)
ttest q81 == 0.66
ttest q81 == 0.66 if (donatore == 1)
ttest q81 == 0.66 if (donatore == 0)
ttest q81 == 0.5 if (donatore == 0 & propensoadonare == 1)
ttest q81 == 0.33 if (donatore == 0 & propensoadonare == 0)
ttest q81 = 0.33 if q57==0| q60_si == 0
restore

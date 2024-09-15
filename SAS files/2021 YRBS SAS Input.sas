/******************************************************************************************/
/* This is the second of two SAS Scripts needed to prepare the 2021 National YRBS dataset */
/* for analysis in the SOGI Project (SM Goodreau, MP Barry). These files must be run in   */
/* sequence. The first is named "2021 YRBS SAS Formats.sas"                               */
/*                                                                                        */
/* This file will generatea .csv file and a SAS dataset file to run from a loca folder.   */
/* Subsequent scripts in this GitHub project use R and require the .csv file.             */
/*                                                                                        */
/*                                   *** BEFORE RUNNING ***                               */
/*  Ensure that "2021 YRBS SAS Formats.sas" was run and generated a format object in the  */
/*  appropriate local file location. Change the file location from 'H:\mpbarry\data' to   */
/*  the location where you want the format library to be stored before you run this       */
/*  program. Do this by changing the location in the 'library' statement at the top of    */
/*  the program.                                                                          */
/******************************************************************************************/
 
filename datain 'H:\mpbarry\data\2021_YRBS_Data.dat';
libname dataout 'H:\mpbarry\data';
libname library 'H:\mpbarry\data';
data dataout.yrbs2021 ;
infile datain lrecl=500;
Input
@17 Q1 $1.
@18 Q2 $1.
@19 Q3 $1.
@20 Q4 $1.
@21 Q5 $8.
@29 Q6 4.2
@33 Q7 6.2
@39 Q8 $1.
@40 Q9 $1.
@41 Q10 $1.
@42 Q11 $1.
@43 Q12 $1.
@44 Q13 $1.
@45 Q14 $1.
@46 Q15 $1.
@47 Q16 $1.
@48 Q17 $1.
@49 Q18 $1.
@50 Q19 $1.
@51 Q20 $1.
@52 Q21 $1.
@53 Q22 $1.
@54 Q23 $1.
@55 Q24 $1.
@56 Q25 $1.
@57 Q26 $1.
@58 Q27 $1.
@59 Q28 $1.
@60 Q29 $1.
@61 Q30 $1.
@62 Q31 $1.
@63 Q32 $1.
@64 Q33 $1.
@65 Q34 $1.
@66 Q35 $1.
@67 Q36 $1.
@68 Q37 $1.
@69 Q38 $1.
@70 Q39 $1.
@71 Q40 $1.
@72 Q41 $1.
@73 Q42 $1.
@74 Q43 $1.
@75 Q44 $1.
@76 Q45 $1.
@77 Q46 $1.
@78 Q47 $1.
@79 Q48 $1.
@80 Q49 $1.
@81 Q50 $1.
@82 Q51 $1.
@83 Q52 $1.
@84 Q53 $1.
@85 Q54 $1.
@86 Q55 $1.
@87 Q56 $1.
@88 Q57 $1.
@89 Q58 $1.
@90 Q59 $1.
@91 Q60 $1.
@92 Q61 $1.
@93 Q62 $1.
@94 Q63 $1.
@95 Q64 $1.
@96 Q65 $1.
@97 Q66 $1.
@98 Q67 $1.
@99 Q68 $1.
@100 Q69 $1.
@101 Q70 $1.
@102 Q71 $1.
@103 Q72 $1.
@104 Q73 $1.
@105 Q74 $1.
@106 Q75 $1.
@107 Q76 $1.
@108 Q77 $1.
@109 Q78 $1.
@110 Q79 $1.
@111 Q80 $1.
@112 Q81 $1.
@113 Q82 $1.
@114 Q83 $1.
@115 Q84 $1.
@116 Q85 $1.
@117 Q86 $1.
@118 Q87 $1.
@119 Q88 $1.
@120 Q89 $1.
@121 Q90 $1.
@122 Q91 $1.
@123 Q92 $1.
@124 Q93 $1.
@125 Q94 $1.
@126 Q95 $1.
@127 Q96 $1.
@128 Q97 $1.
@129 Q98 $1.
@130 Q99 $1.
@185 QN8 1.
@186 QN9 1.
@187 QN10 1.
@188 QN11 1.
@189 QN12 1.
@190 QN13 1.
@191 QN14 1.
@192 QN15 1.
@193 QN16 1.
@194 QN17 1.
@195 QN18 1.
@196 QN19 1.
@197 QN20 1.
@198 QN21 1.
@199 QN22 1.
@200 QN23 1.
@201 QN24 1.
@202 QN25 1.
@203 QN26 1.
@204 QN27 1.
@205 QN28 1.
@206 QN29 1.
@207 QN30 1.
@208 QN31 1.
@209 QN32 1.
@210 QN33 1.
@211 QN34 1.
@212 QN35 1.
@213 QN36 1.
@214 QN37 1.
@215 QN38 1.
@216 QN39 1.
@217 QN40 1.
@218 QN41 1.
@219 QN42 1.
@220 QN43 1.
@221 QN44 1.
@222 QN45 1.
@223 QN46 1.
@224 QN47 1.
@225 QN48 1.
@226 QN49 1.
@227 QN50 1.
@228 QN51 1.
@229 QN52 1.
@230 QN53 1.
@231 QN54 1.
@232 QN55 1.
@233 QN56 1.
@234 QN57 1.
@235 QN58 1.
@236 QN59 1.
@237 QN60 1.
@238 QN61 1.
@239 QN62 1.
@240 QN63 1.
@243 QN66 1.
@244 QN67 1.
@245 QN68 1.
@246 QN69 1.
@247 QN70 1.
@248 QN71 1.
@249 QN72 1.
@250 QN73 1.
@251 QN74 1.
@252 QN75 1.
@253 QN76 1.
@254 QN77 1.
@255 QN78 1.
@256 QN79 1.
@257 QN80 1.
@258 QN81 1.
@259 QN82 1.
@260 QN83 1.
@261 QN84 1.
@262 QN85 1.
@263 QN86 1.
@264 QN87 1.
@265 QN88 1.
@266 QN89 1.
@267 QN90 1.
@268 QN91 1.
@269 QN92 1.
@270 QN93 1.
@271 QN94 1.
@272 QN95 1.
@273 QN96 1.
@274 QN97 1.
@275 QN98 1.
@276 QN99 1.
@1 SITE $3.
@350 QNFRCIG 1.
@351 QNDAYCIG 1.
@352 QNFREVP 1.
@353 QNDAYEVP 1.
@354 QNFRSKL 1.
@355 QNDAYSKL 1.
@356 QNFRCGR 1.
@357 QNDAYCGR 1.
@358 QNTB2 1.
@359 QNTB3 1.
@360 QNTB4 1.
@361 QNTB5 1.
@362 QNIUDIMP 1.
@363 QNOTHHPL 1.
@364 QNDUALBC 1.
@365 QNBCNONE 1.
@366 QNFR0 1.
@367 QNFR1 1.
@368 QNFR2 1.
@369 QNVEG0 1.
@370 QNVEG1 1.
@371 QNVEG2 1.
@372 QNVEG3 1.
@373 QNSODA1 1.
@374 QNSODA2 1.
@375 QNMILK1 1.
@376 QNMILK3 1.
@377 QNBK7DAY 1.
@378 QNPA0DAY 1.
@379 QNPA7DAY 1.
@380 QNDLYPE 1.
@381 QNNODNT 1.
@382 QNSPDRK1 1.
@383 QNSPDRK2 1.
@384 QNWATER1 1.
@385 QNWATER2 1.
@386 QNWATER3 1.
@387 QNILLICT 1.
@388 QNOBESE 1.
@389 QNOWT 1.
@390 WEIGHT 10.4
@400 STRATUM 3.
@403 PSU 6.
@409 BMIPCT 5.2
@414 RACEETH $2.
@416 Q6ORIG $3.
@419 Q7ORIG $3.
;

/****************************************/
/*   Assign formats to SAS variables    */
/****************************************/
 
format
Q1 $H1S. Q2 $H2S. Q3 $H3S. Q4 $H4S. Q5 $char8. 
Q6 4.2 Q7 6.2 Q8 $H8S. Q9 $H9S. Q10 $H10S. 
Q11 $H11S. Q12 $H12S. Q13 $H13S. Q14 $H14S. Q15 $H15S. 
Q16 $H16S. Q17 $H17S. Q18 $H18S. Q19 $H19S. Q20 $H20S. 
Q21 $H21S. Q22 $H22S. Q23 $H23S. Q24 $H24S. Q25 $H25S. 
Q26 $H26S. Q27 $H27S. Q28 $H28S. Q29 $H29S. Q30 $H30S. 
Q31 $H31S. Q32 $H32S. Q33 $H33S. Q34 $H34S. Q35 $H35S. 
Q36 $H36S. Q37 $H37S. Q38 $H38S. Q39 $H39S. Q40 $H40S. 
Q41 $H41S. Q42 $H42S. Q43 $H43S. Q44 $H44S. Q45 $H45S. 
Q46 $H46S. Q47 $H47S. Q48 $H48S. Q49 $H49S. Q50 $H50S. 
Q51 $H51S. Q52 $H52S. Q53 $H53S. Q54 $H54S. Q55 $H55S. 
Q56 $H56S. Q57 $H57S. Q58 $H58S. Q59 $H59S. Q60 $H60S. 
Q61 $H61S. Q62 $H62S. Q63 $H63S. Q64 $H64S. Q65 $H65S. 
Q66 $H66S. Q67 $H67S. Q68 $H68S. Q69 $H69S. Q70 $H70S. 
Q71 $H71S. Q72 $H72S. Q73 $H73S. Q74 $H74S. Q75 $H75S. 
Q76 $H76S. Q77 $H77S. Q78 $H78S. Q79 $H79S. Q80 $H80S. 
Q81 $H81S. Q82 $H82S. Q83 $H83S. Q84 $H84S. Q85 $H85S. 
Q86 $H86S. Q87 $H87S. Q88 $H88XX. Q89 $H89XX. Q90 $H90XX. 
Q91 $H91XX. Q92 $H92XX. Q93 $H93XX. Q94 $H94XX. Q95 $H95XX. 
Q96 $H96XX. Q97 $H97XX. Q98 $H98XX. Q99 $H99XX. 
raceeth $HRCE.
;

/****************************************/
/*   Assign labels to SAS variables     */
/****************************************/
 
label
Q1="How old are you"
Q2="What is your sex"
Q3="In what grade are you"
Q4="Are you Hispanic/Latino"
Q5="What is your race"
Q6="How tall are you"
Q7="How much do you weigh"
Q8="Seat belt use"
Q9="Riding with a drinking driver"
Q10="Drinking and driving"
Q11="Texting and driving"
Q12="Weapon carrying at school"
Q13="Gun carrying"
Q14="Safety concerns at school"
Q15="Threatened at school"
Q16="Times in fight past 12 months"
Q17="Physical fighting at school"
Q18="Saw physical violence in neighborhood"
Q19="Ever been physically forced sexual intercourse"
Q20="Sexual violence"
Q21="Sexual dating violence"
Q22="Physical dating violence"
Q23="Bullied at school"
Q24="Electronic bullying"
Q25="Sad or hopeless"
Q26="Considered suicide"
Q27="Made a suicide plan past 12 months"
Q28="Actually attempted suicide"
Q29="Injurious suicide attempt"
Q30="Ever cigarette use"
Q31="Initiation of cigarette smoking"
Q32="Current cigarette use"
Q33="Smoked >10 cigarettes"
Q34="Electronic vapor product use"
Q35="Current electronic vapor product use"
Q36="EVP from store"
Q37="Current smokeless tobacco use"
Q38="Current cigar use"
Q39="All tobacco product cessation"
Q40="Initiation of alcohol use"
Q41="Current alcohol use"
Q42="Current binge drinking"
Q43="Drinks in a row past 30 days"
Q44="Source of alcohol"
Q45="Ever marijuana use"
Q46="Initiation of marijuana use"
Q47="Current marijuana use"
Q48="Ever synthetic marijuana use"
Q49="Ever prescription pain medicine use"
Q50="Ever cocaine use"
Q51="Ever inhalant use"
Q52="Ever heroin use "
Q53="Ever methamphetamine use"
Q54="Ever ecstasy use"
Q55="Illegal injected drug use"
Q56="Illegal drugs at school"
Q57="Ever sexual intercourse"
Q58="Sex before 13 years"
Q59="Number of sex partners"
Q60="Current sexual activity"
Q61="Alcohol/drugs and sex"
Q62="Condom use"
Q63="Birth control pill use"
Q64="Sex of sexual contacts"
Q65="Sexual identity"
Q66="Perception of weight"
Q67="Weight loss"
Q68="Fruit juice drinking"
Q69="Fruit eating"
Q70="Green salad eating"
Q71="Potato eating"
Q72="Carrot eating"
Q73="Other vegetable eating"
Q74="No soda drinking"
Q75="No milk drinking"
Q76="Breakfast eating"
Q77="Physical activity >= 5 days"
Q78="Screen time"
Q79="PE attendance"
Q80="Sports team participation"
Q81="Concussion"
Q82="HIV testing"
Q83="STD testing"
Q84="Oral health care"
Q85="Current mental health"
Q86="Hours of sleep on school night"
Q87="Homelessness"
Q88="Current Rx Pain med w/out Rx"
Q89="Ever used hallucinogenic drugs"
Q90="Sports drinks"
Q91="Plain water"
Q92="Muscle strengthening"
Q93="Mental health status COVID-19"
Q94="Parental job loss COVID-19"
Q95="Sunburn"
Q96="Feel close to people at their school"
Q97="Adult know whereabouts"
Q98="Difficulty concentrating"
Q99="How well speak English"
QN8="Did not always wear a seat belt"
QN9="Rode with a driver who had been drinking alcohol"
QN10="Drove a car or other vehicle when they had been drinking alcohol"
QN11="Texted or e-mailed while driving a car or other vehicle"
QN12="Carried a weapon on school property "
QN13="Carried a gun "
QN14="Did not go to school because they felt unsafe at school or on their way to or from school "
QN15="Were threatened or injured with a weapon on school property "
QN16="Were in a physical fight "
QN17="Were in a physical fight on school property "
QN18="Ever saw someone get physically attacked, beaten, stabbed, or shot in their neighborhood"
QN19="Were ever physically forced to have sexual intercourse"
QN20="Experienced sexual violence"
QN21="Experienced sexual dating violence"
QN22="Experienced physical dating violence"
QN23="Were bullied on school property"
QN24="Were electronically bullied"
QN25="Felt sad or hopeless"
QN26="Seriously considered attempting suicide"
QN27="Made a plan about how they would attempt suicide"
QN28="Attempted suicide"
QN29="Had a suicide attempt that resulted in an injury, poisoning, or overdose that had to be treated by a doctor or nurse"
QN30="Ever tried cigarette smoking"
QN31="First tried cigarette smoking before age 13 years"
QN32="Currently smoked cigarettes"
QN33="Smoked more than 10 cigarettes per day"
QN34="Ever used an electronic vapor product"
QN35="Currently used an electronic vapor product"
QN36="Usually got their electronic vapor products by buying them themselves in a convenience store, supermarket, discount store, or gas station"
QN37="Currently used smokeless tobacco"
QN38="Currently smoked cigars"
QN39="Tried to quit using all tobacco products"
QN40="Had their first drink of alcohol before age 13 years"
QN41="Currently drank alcohol"
QN42="Currently were binge drinking"
QN43="Reported that the largest number of drinks they had in a row was 10 or more"
QN44="Usually got the alcohol they drank by someone giving it to them"
QN45="Ever used marijuana "
QN46="tried marijuana for the first time before age 13 years"
QN47="Currently used marijuana "
QN48="Ever used synthetic marijuana"
QN49="Ever took prescription pain medicine without a doctor's prescription or differently than how a doctor told them to use it"
QN50="Ever used cocaine "
QN51="Ever used inhalants"
QN52="Ever used heroin "
QN53="Ever used methamphetamines"
QN54="Ever used ecstasy"
QN55="Ever injected any illegal drug "
QN56="Were offered, sold, or given an illegal drug on school property"
QN57="Ever had sexual intercourse"
QN58="Had sexual intercourse for the first time before age 13 years"
QN59="Had sexual intercourse with four or more persons during their life"
QN60="Were currently sexually active"
QN61="Drank alcohol or used drugs before last sexual intercourse"
QN62="Used a condom during last sexual intercourse"
QN63="Used birth control pills before last sexual intercourse with opposite-sex partner"
QN66="Described themselves as slightly or very overweight"
QN67="Were trying to lose weight"
QN68="Did not drink fruit juice"
QN69="Did not eat fruit"
QN70="Did not eat green salad"
QN71="Did not eat potatoes"
QN72="Did not eat carrots"
QN73="Did not eat other vegetables"
QN74="Did not drink a can, bottle, or glass of soda or pop "
QN75="Did not drink milk"
QN76="Did not eat breakfast"
QN77="Were physically active at least 60 minutes per day on 5 or more days"
QN78="Spent 3 or more hours per day on screen time"
QN79="Attended physical education (PE) classes on 1 or more days"
QN80="Played on at least one sports team"
QN81="Had a concussion from playing a sport or being physically active"
QN82="Were ever tested for human immunodeficiency virus (HIV)"
QN83="Were ever tested for a sexually transmitted disease (STD)"
QN84="Saw a dentist"
QN85="Reported that their mental health was most of the time or always not good"
QN86="Got 8 or more hours of sleep"
QN87="Usually did not sleep in their parent's or guardian's home"
QN88="currently took prescription pain medicine without a doctor''s prescription or differently than how a doctor told them to use it"
QN89="ever used hallucinogenic drugs"
QN90="did not drink a can, bottle, or glass of a sports drink"
QN91="did not drink a bottle or glass of plain water"
QN92="did exercises to strengthen or tone their muscles on three or more days"
QN93="Reported that their mental health was most of the time or always not good during the COVID-19 pandemic"
QN94="Reported that their parent or other adult in their home lost their job during the COVID-19 pandemic"
QN95="had a sunburn"
QN96="strongly agree or agree that they feel close to people at their school"
QN97="reported that their parents or other adults in their family most of the time or always know where they are going or with whom they will be"
QN98="have serious difficulty concentrating, remembering, or making decisions"
QN99="speak English well or very well"
site="Site Code"
qnfrcig="Currently smoked cigarettes frequently "
qndaycig="Currently smoked cigarettes daily"
qnfrevp="Currently used electronic vapor products frequently"
qndayevp="Currently used electronic vapor products daily"
qnfrskl="Currently used smokeless tobacco frequently"
qndayskl="Currently used smokeless tobacco daily"
qnfrcgr="Currently smoked cigars frequently"
qndaycgr="Currently smoked cigars daily"
qntb2="Currently smoked cigarettes or cigars"
qntb3="Currently smoked cigarettes or cigars or used smokeless tobacco"
qntb4="Currently smoked cigarettes or cigars or used smokeless tobacco or electronic vapor products"
qntb5="Currently smoked cigarettes or used electronic vapor products"
qniudimp="Used an IUD (such as Mirena or ParaGard) or implant (such as Implanon or Nexplanon) before last sexual intercourse with an opposite-sex partner"
qnothhpl="Used birth control pills; an IUD or implant; or a shot, patch, or birth control ring before last sexual intercourse with an opposite-sex partner"
qndualbc="Used both a condom during last sexual intercourse and birth control pills; an IUD or implant; or a shot, patch, or birth control ring before last sexual intercourse with an opposite-sex partner"
qnbcnone="Did not use any method to prevent pregnancy during last sexual intercourse with an opposite-sex partner"
qnfr0="Did not eat fruit or drink 100% fruit juices"
qnfr1="Ate fruit or drank 100% fruit juices one or more times per day"
qnfr2="Ate fruit or drank 100% fruit juices two or more times per day"
qnveg0="Did not eat vegetables"
qnveg1="Ate vegetables one or more times per day"
qnveg2="Ate vegetables two or more times per day"
qnveg3="Ate vegetables three or more times per day"
qnsoda1="Drank a can, bottle, or glass of soda or pop one or more times per day"
qnsoda2="Drank a can, bottle, or glass of soda or pop two or more times per day"
qnmilk1="Drank one or more glasses per day of milk"
qnmilk3="Drank three or more glasses per day of milk"
qnbk7day="Ate breakfast on all 7 days"
qnpa0day="Did not participate in at least 60 minutes of physical activity on at least 1 day"
qnpa7day="Were physically active at least 60 minutes per day on all 7 days"
qndlype="Attended physical education classes on all 5 days"
qnnodnt="Never saw a dentist"
qnspdrk1="Drank a can, bottle, or glass of a sports drink one or more times per day"
qnspdrk2="Drank a can, bottle, or glass of a sports drink two or more times per day"
qnwater1="Drank a bottle or glass of plain water one or more times per day"
qnwater2="drank a bottle or glass of plain water two or more times per day"
qnwater3="drank a bottle or glass of plain water three or more times per day"
qnillict="Ever used select illicit drugs"
qnobese="Had obesity"
qnowt="Were Overweight"
weight="Overall Analysis Weight"
stratum="Sampling Strata"
psu="Primary Sampling Unit"
bmipct="Body Mass Index Percentile"
raceeth="Race/Ethnicity"
q6orig="Original unedited student height"
q7orig="Original unedited student weight"
;
run;

/******************************************/
/**** Generate a CSV for easy use in R ****/
/******************************************/
proc export data=library.yrbs2021
    outfile="'H:\mpbarry\data\yrbs2021.csv"
    dbms=csv;
run;

* Encoding: UTF-8.
Opgave 1:
    
*Inconsistentiecheck.

FREQUENCIES VARIABLES=W2_Gebjaar W2_Corstress1 W2_Corstress2 W2_Corstress3 W2_Corstress4
  /ORDER=ANALYSIS.

COMPUTE W2_Leeftijd=2021-W2_Gebjaar.
EXECUTE.

COMPUTE W2_Corstress_Gem=MEAN(W2_Corstress1,W2_Corstress2,W2_Corstress3,W2_Corstress4).
VARIABLE LABELS  W2_Corstress_Gem 'Gemiddelde score op vier items over coronastress'.
EXECUTE.

*Residuenanalyse en regressie.

REGRESSION
  /DESCRIPTIVES MEAN STDDEV CORR SIG N
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT W2_Corstress_Gem
  /METHOD=ENTER W2_Leeftijd
  /SCATTERPLOT=(*ZRESID ,*ZPRED)
  /RESIDUALS DURBIN HISTOGRAM(ZRESID)
  /CASEWISE PLOT(ZRESID) OUTLIERS(3).

Opgave 2
    
*Inconsistentiecheck.
    
USE ALL.
COMPUTE filter_$=(W2_Relatiestatus = 1).
VARIABLE LABELS filter_$ 'W2_Relatiestatus = 1 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.
    
COMPUTE W2_Steun_partner_Gem=MEAN(W2_Steun_partner1, W2_Steun_partner2, W2_Steun_partner3, W2_Steun_partner4, W2_Steun_partner5).
VARIABLE LABELS  W2_Steun_partner_Gem 'Gemiddelde score op vijf items over steun van de partner'.
EXECUTE.

COMPUTE W2_QMI_Gem=MEAN(W2_QMI1,W2_QMI2,W2_QMI3,W2_QMI4,W2_QMI5).
VARIABLE LABELS  W2_QMI_Gem 'Gemiddelde score op vijf items over relatietevredenheid'.
EXECUTE.

COMPUTE W2_Corstress_Gem=MEAN(W2_Corstress1,W2_Corstress2,W2_Corstress3,W2_Corstress4).
VARIABLE LABELS  W2_Corstress_Gem 'Gemiddelde score op vier items over coronastress'.
EXECUTE.

*Assumpties

RECODE W2_DIPLOMA (1=1) (MISSING=SYSMIS) (ELSE=0) INTO Diploma_Dummy_1.
VARIABLE LABELS  Diploma_Dummy_1 'Geen of laag diploma vs hoger onderwijs'.
EXECUTE.
RECODE W2_DIPLOMA (2=1) (MISSING=SYSMIS) (ELSE=0) INTO Diploma_Dummy_2.
VARIABLE LABELS  Diploma_Dummy_2 'Middelbaar onderwijs vs hoger onderwijs'.
EXECUTE.

FREQUENCIES VARIABLES= W2_DIPLOMA Diploma_Dummy_1 Diploma_Dummy_2
  /ORDER=ANALYSIS.

*Regressie
    
REGRESSION
  /DESCRIPTIVES MEAN STDDEV CORR SIG N
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS CI(95) R ANOVA COLLIN TOL CHANGE
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT W2_Corstress_Gem
  /METHOD=ENTER Diploma_Dummy_1 Diploma_Dummy_2 W2_Geslacht W2_Handicap
  /METHOD=ENTER W2_QMI_Gem W2_Steun_partner_Gem
  /SCATTERPLOT=(*ZRESID ,*ZPRED)
  /RESIDUALS DURBIN HISTOGRAM(ZRESID)
  /CASEWISE PLOT(ZRESID) OUTLIERS(3).


# This is a R script with some functions 


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
EconomicIndicators <- function(
	MC.Simulation = 0, 
	latest.sd = 1,
	starting.year = 2025,
	Inflation.Forecast = c(2.1, 2.1, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0),
	Unemployment.Forecast = c(6.0, 6.1, 6.0, 6.0, 5.9, 5.9, 5.9, 5.9, 5.9, 5.9),
	holding.period = 10
	){
  # Creates macro economic forecasts for several indicators as MC variables on purpose
	# KPIs: 
  #
  # Args:
  #      xdata: 
  #  
  # Fixed Args:
  #      
  #
  # Returns:  
  # 
  #-----------------------
	
## Step A: Plausability checks
	{
		if(length(Inflation.Forecast) != holding.period)
			return(print("Warning: lengths of indicators differ!"))
	}
	
## Step 1: Historical data and modelling
	{
		## Step 1.1: Historical Data loading and data for leading forecast variable
		{
		macro_data <- read.csv("C:/Analyse/Projekte/NERVE_Modeling/Macro_Data_2024.csv", header = TRUE, dec = ",", sep = ";") 
		
		# Generate sample data (1963-2024)
		MIR <- macro_data$MR_Discont[13:nrow(macro_data)] # monetary interest rate
		CPI <- macro_data$CPI_growths[13:nrow(macro_data)] # comsumer price index
		Unempl <- macro_data$Unemployment[13:nrow(macro_data)] # unemployment rate
		CCI <- macro_data$CCI_growths[13:nrow(macro_data)] # construction cost index
		HPI <- macro_data$HP_growths[13:nrow(macro_data)] # housing price index
		DisInc <- macro_data$DisInc[13:nrow(macro_data)] # disp. income growth
		HMR_5yr <- macro_data$HMR_5yr[13:nrow(macro_data)] # housing mortgage rate
		BR_10yr <- macro_data$BR_10yr[13:nrow(macro_data)] # bond rate
		
		# Create time series object
		MIR_ts <- ts(MIR, frequency = 1, c(1963:max(macro_data$Year)))
		HPI_ts <- ts(HPI, frequency = 1, c(1963:max(macro_data$Year)))
		DisInc_ts <- ts(DisInc, frequency = 1, c(1963:max(macro_data$Year)))
		HMR_5yr_ts <- ts(HMR_5yr, frequency = 1, c(1963:max(macro_data$Year)))
		CCI_ts <- ts(CCI, frequency = 1, c(1963:max(macro_data$Year)))
		BR_10yr_ts <- ts(BR_10yr, frequency = 1, c(1963:max(macro_data$Year)))
}
			
		## Step 2.1: Data Modelling and saving results
		{
		# Automated model fitting by 
		model.fit.mir <- auto.arima(MIR_ts, xreg = CPI) 
		model.fit.cci <- auto.arima(CCI_ts, xreg = CPI) 
		model.fit.hmr <- auto.arima(HMR_5yr_ts, xreg = MIR) 
		model.fit.dis.inc <- auto.arima(DisInc_ts[38:62], xreg = CPI[38:62]) 
		#model.fit.dis.inc <- forecast::Arima(DisInc_ts[38:62], order = c(2, 1, 0), xreg = matrix(c(CPI[38:62], Unempl[38:62]), ncol = 2))
		#model.fit.dis.inc <- forecast::Arima(DisInc_ts, order = c(1, 1, 0), xreg = matrix(c(CPI), ncol = 1))
		model.fit.br10yr <- auto.arima(BR_10yr_ts, xreg = MIR) 
		
		# Checking the residuals and the model fit
			# summary(model.fit.dis.inc)
			# checkresiduals(model.fit.mir)	
}	
	}
	
## Step 2: Forecast and Save the variable by using forecasted external regressor data
	{
	# Set the sd for CPI (as long term (20 yrs) mean or latest historical value)
	sd_value <- ifelse(MC.Simulation == 1, 
											ifelse(latest.sd == 1, CPI_sd_short, CPI_sd_long), 
											0)
	
	# Simulate the main indicator: inflation (fixed with random error)
	upcoming_CPI <- Inflation.Forecast
	upcoming_CPI[1] <- rnorm(1, mean = Inflation.Forecast[1], sd = sd_value)
	
	# Develop the time series for CPI by the predicted development in the mean
	for (i in 2:holding.period) {
		# Development of mean by prediction
	upcoming_CPI[i] <- upcoming_CPI[i-1] * (1 + calc_cagr(Inflation.Forecast, 1)[i])
	# Add on uncertainty
	upcoming_CPI[i] <- rnorm(1, mean = upcoming_CPI[i], sd = sd_value)
	}
	
	# Or by a time seres model when no definite prediction exists (TO BE ADDED)
	}
	
## Step 3: Use inflation time series for forecasting MIR / CCI
	{
	forecast.mir <- as.data.frame(forecast(model.fit.mir, xreg = upcoming_CPI))
	rownames(forecast.mir) <- c(start.year:(start.year+holding.period-1))
	
	forecast.cci <- as.data.frame(forecast(model.fit.cci, xreg = upcoming_CPI))
	rownames(forecast.cci) <- c(start.year:(start.year+holding.period-1))
	
	# Step 3: Use inflation time series for forecasting MIR and use that to forecast HMR 5-years 
	forecast.hmr <- as.data.frame(forecast(model.fit.hmr, xreg = forecast.mir$`Point Forecast`))
	rownames(forecast.hmr) <- c(start.year:(start.year+holding.period-1))

	forecast.br10yr <- as.data.frame(forecast(model.fit.br10yr, xreg = forecast.mir$`Point Forecast`))
	rownames(forecast.br10yr) <- c(start.year:(start.year+holding.period-1))
	
	# Step 4: Use inflation time series and Unemplo for forecasting dispos. income 
	#forecast.dis.inc <- as.data.frame(forecast(model.fit.dis.inc, xreg = matrix(c(upcoming_CPI, Unemployment.Forecast), ncol = 2)))
	forecast.dis.inc <- as.data.frame(forecast(model.fit.dis.inc, xreg = forecast.mir$`Point Forecast`))
	rownames(forecast.dis.inc) <- c(start.year:(start.year+holding.period-1))
	}
	
## Step 4: Merge return data
	{
		return(data.frame(
			Jahr = (starting.year:(starting.year+holding.period-1)),
			CPI.Forecast = upcoming_CPI,
			MIR.Forecast = forecast.mir$`Point Forecast`,
			CCI.Forecast = forecast.cci$`Point Forecast`,
			HMR.Forecast = forecast.hmr$`Point Forecast`,
			DisInc.Forecast = forecast.dis.inc$`Point Forecast`
		))
	}
}
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
{
	
## README
{
# Errors should be raised using stop()

	## Laden der Daten (diese sind vorher durch Daten und Valuation Schleife gelaufen - Lücken gefüllt)

	# 		i) Unterschiedliche Verträge später ergänzen
	#     ii) Mieterspezifische Fluktuation später: 
					# Marktdelta verringert Umzugswahrscheinlichkeit
					# Höheres Alter verringert Umzugswahrscheinlichkeit (weil keine Nachfrageänderung mehr)
	#     iii) Specific CO2-Cost (für mietbelastung relevant)
  #     iii) Haushaltsstrom inkludieren
	#     iv) Mietbelastung mit fester Wohnfläche pro Person (durchschnittlich Haushaltsgröße runterbrechen)
	#     fixe mietbelastungsschwelle einbauen?
	#     v) Nebenkosten mit Zustand und Größe verbinden (aus Marktdaten als Input)
  #     VI) Dynamische Entwicklung der Leerstandsquote (über Prognose Angebot und Nachfrage)
	#     VII) Dynamische Entwicklung der Marktfluaktion (über Entwicklung Leerstand und Angebot abhängig)
  #         Mietentwicklung bei Bestandsmieten auch von Marktsituation abhängig
}
Inputs <- read.xlsx("C:/Analyse/Projekte/NERVE_Modeling/Inputs.xlsx", sheetIndex = 1, header = TRUE, encoding = "UTF-8") # load data
Rent.Roll <- read.xlsx("C:/Analyse/Projekte/NERVE_Modeling/Inputs.xlsx", sheetIndex = 2, header = TRUE, encoding = "UTF-8") # load data
	
	
ERV.Calculation <- function(	
	sim.n = 20,
	property.no = 2,
	unit.no = 1
	) {
  # Uses given rent roll data and valuation results to create ERVs on different bases
  #
  # Args:
  #   sim.n: number of loops when simulation the ERV
  #   property.no: number of regared property within list
  #   unit.no: number of regarded unit within list
	#
	# Requirements:
	#   EconomicIndicators: creating forecasts of economic indicators (based on modelled historic data)
	#
	# Global Args: 
	#   Inputs:
	#   Rent Roll: 
  #
  # Returns:
  #   vector for different rent growths scenarios, their error margins as well as relevant related working parameter
	#
  #-----------------------

	## Full Description
	{
	# Hier kommt eine ausführliche Beschreibung hin. Zur Methodik und den Annahmen	
	}
	
	## Initials: Set unit data
	{
	## Stop when global data is not loaded
	if(isEmpty(Rent.Roll) == TRUE){
		stop("Input data is not loaded to global environment")
	}
	
	## Loading unit data from global inputs data table
	temp.Rent.Roll <- filter(Rent.Roll, property == property.no & unit == unit.no)
	temp.Inputs <- filter(Inputs, property == property.no)
	}

	#-----------------------

	## Step 1: Input variables Initialization and Settings
	{
	# Desc.: For more details on the different variables see the related glossary	
		
	### I.1) General Settings
	{
	# General Settings (all variables as single entry)
	holding.period <- as.numeric(temp.Inputs$holding.period) # Mand. Input
	start.year <- as.numeric(temp.Inputs$year) # Mand. Input
	}
		
  ### I.2) Object Inputs
	{
	# Step 1): Object characteristics (all variables as vector over definite time span) - load only relevant ones
	unit.size <- as.numeric(rep(temp.Rent.Roll$unit.size, holding.period)) # Mand. Input
	object.warm.util.cost <- rep(1.8, holding.period) # Object Valua. and Dev. / Opt. Input
	object.cold.util.cost <- rep(1.6, holding.period) # Object Valua. and Dev. / Opt. Input
	
	# Step 2): In-place-Rent information
	in.place.rent.sqm <- as.numeric(rep(temp.Rent.Roll$rent.level, holding.period)) # Mand. Input
	in.place.rent.sqm.B <- in.place.rent.sqm
	in.place.rent.sqm.C <- in.place.rent.sqm

	# Step 3): Modernization plans
	modernization.cost <- as.numeric(rep(temp.Rent.Roll$modernization.cost, holding.period)) # Mand. Input
	modernization.period <- as.numeric(rep(temp.Rent.Roll$modernization.period)) # Mand. Input
	modernization.periods <- rep(0, holding.period)
	modernization.periods[modernization.period] <- 1
	modernization.rent.discount <- as.numeric(rep(temp.Rent.Roll$modernization.rent.discount, holding.period)) # Mand. Input
	valuation.rent.modernization <- as.numeric(rep(temp.Rent.Roll$valuation.rent.modernization, holding.period)) # Mand. Input
	}
	
	### I.3) General local and global market data
	{
	
	# Step 1) Global Market inputs: 
	cci.total <- rep(0.02, holding.period) # Global Market data and Dev. // FOR TESTS ONLY
	cpi.total <- rep(0.03, holding.period) # Global Market data and Dev. // FOR TESTS ONLY
	cpi.opex <- rep(0.03, holding.period) # Global Market data and Dev. // FOR TESTS ONLY
	cpi.utils.cost.cold <- rep(0.03, holding.period) # Global Market data and Dev. // FOR TESTS ONLY
	cpi.utils.cost.warm <- rep(0.03, holding.period) # Global Market data and Dev. // FOR TESTS ONLY
	bond.yield <- c(0.024, rep(NA, holding.period-1)) # Global Market data and Dev. // FOR TESTS ONLY
	forward.curve <- rep(0.03, holding.period) # Global Market data and Dev. // FOR TESTS ONLY
	 
	# Step 2) Local rental market data
	rent.market.situation <- as.character(rep(temp.Inputs$rent.market.situation, holding.period)) # Mand. Input
	market.warm.util.cost <- rep(1.9, holding.period) # Specific Market data (for local market) and Dev.
	market.cold.util.cost <- rep(1.7, holding.period) # Specific Market data (for local market) and Dev.
	market.cap.limit <- as.numeric(rep(temp.Inputs$market.cap.limit, holding.period))# Mand. Input
	market.rent <- as.numeric(rep(temp.Rent.Roll$market.data, holding.period)) # Market data for specific size class and Dev.: general market level
	##
	market.unit.size <-  as.numeric(rep(temp.Inputs$market.unit.size, holding.period)) # Same as object
	market.household.size <- as.numeric(rep(temp.Inputs$market.household.size, holding.period)) # Market data
	market.vacancy.rate <- as.numeric(rep(temp.Inputs$market.vacancy.rate, holding.period)) # Market data and Dev.
	market.churn.rate <- as.numeric(rep(temp.Inputs$market.churn.rate, holding.period)) # Market data and Dev.
	market.household.income <- as.numeric(rep(temp.Inputs$market.household.income, holding.period)) # Market data and Dev.
	income.growth <- as.numeric(rep(temp.Inputs$income.growth, holding.period)) # Market data and Dev. / MC /
}

	### I.4) Object specific calculations and valuations
	{
	object.market.rent <- as.numeric(rep(temp.Rent.Roll$valuation.rent, holding.period)) # Object Valua. and Dev.
	object.mietspiegel.rent <- as.numeric(rep(temp.Rent.Roll$mietspiegel.rent, holding.period)) # Object Valua. and Dev.
	object.churn.rate <- as.numeric(rep(temp.Rent.Roll$churn.rate, holding.period)) # Mand. Input 
	object.vacancy.rate <- as.numeric(rep(temp.Rent.Roll$vacant.status, holding.period))  # Mand. Input 
	#
	object.household.income <- as.numeric(rep(temp.Inputs$object.household.income, holding.period))  # Mand. Input and Dev.
}
	
	### I.5) Temporary calculation data (temp.)
	{
	temp.market.affordability.rate <- rep(NA, holding.period) # Second. Calc. and Dev.
	temp.object.affordability.rate <- rep(NA, holding.period) # Second. Calc. and Dev.
	temp.object.occupancy.rate <- c(1 - object.vacancy.rate) # Second. Calc.
	temp.in.place.afford.rent.sqm <- rep(NA, holding.period) # Second. Calc.
	temp.in.place.warm.rent.total <- rep(NA, holding.period) # Second. Calc.
	in.place.cold.rent.total <- rep(NA, holding.period) # Second. Calc.
	net.rental.income.total <- rep(NA, holding.period) # Second. Calc.
	vacancy.loss <- rep(0, holding.period) # Second. Calc.
	churn.loss <- rep(0, holding.period) # Second. Calc.
	opex <- rep(NA, holding.period) # Second. Calc.
	modernization.loss <- rep(0, holding.period) # Second. Calc.
	tenant.shift <- rep(0, holding.period) # ob wechsel stattfefunden haben
  total.rent.sqm  <- rep(0, holding.period)# ne cold
	total.rent.sqm.B <- total.rent.sqm
	total.rent.sqm.C <- total.rent.sqm
 	Mod.umlage <- rep(0, holding.period)  # internal parameter
 	in.place.mod.rent.sqm <- rep(0, holding.period)
 	in.place.mod.rent.growth <- rep(0, holding.period)
 	in.place.mod.rent.growth.B <- in.place.mod.rent.growth
 	in.place.mod.rent.growth.C <- in.place.mod.rent.growth
 	# new-contract rent development
	new.rent.sqm <- rep(0, holding.period)
	new.rent.sqm.B <- rep(0, holding.period)
	new.rent.sqm.C <- rep(0, holding.period)
	}
	
	}
	
	#-----------------------
		
	## Step 2: Pre-Calculations: Market Developments
	{

	## 1) MC-relevant macro indicators
	{
	macro.indicators.data <- EconomicIndicators(MC.Simulation = 0)
	}	

	#-----------------------	Global indicators
	
	## 1) Inflation and construction Cost
		
	# 1 Entwicklung Inflation (insgesamt) - MC-Parameter or fixed
	{
		cpi.total <- macro.indicators.data$CPI.Forecast/100
	}
	
	# 2 Entwicklung Inflation (OpEx relevant) - MC-Parameter or fixed MISSING
	{
		#cpi.opex
	}
	
	#	3 Entwicklung Baukosten (CapEx revelant) - MC-Parameter or fixed
	{
		cci.total <- macro.indicators.data$CCI.Forecast/100
	}
	
	# 4 Entwicklung Nebenkosten (warm) - für Erschwinglichkeit ("cpi.utils.cost.warm"/"market.warm.util.cost")
	{
	# Develop the growth rate "cpi.utils.cost.warm"
		
	# Loop over holding period: 
	for (i in 2:holding.period) {
			market.warm.util.cost[i] <- market.warm.util.cost[i-1] * (1.0 + cpi.utils.cost.warm[i])
	}		
		
	}
	
	# 5 Entwicklung Nebenkosten (kalt) - für Erschwinglichkeit ("cpi.utils.cost.cold"/"market.cold.util.cost")
	{
	# Develop the growth rate "cpi.utils.cost.cold
		
	# Loop over holding period: 
	for (i in 2:holding.period) {
			market.cold.util.cost[i] <- market.cold.util.cost[i-1] * (1.0 + cpi.utils.cost.cold[i])
	}
	}
	
	# 6 Entwicklung Kosten für Haushaltsstrom MISSING
	{
		# LATER
	}
		
	# 7 Entwicklung CO2-Kosten allgemein MISSING
	{		
		}
	
	#-----------------------	Local Market
		
	## 2) Disosable Income, rents and affordability

	# 1 Entwicklung Haushaltseinkommen ("market.household.income")
	{
	#
	# Loop over holding period: 
	for (i in 2:holding.period) {
			market.household.income[i] <- market.household.income[i-1] * (1.0 + income.growth[i])
		}	
	}
	
	# 2 Entwicklung Erwartete Marktmiete ("market.rent") - Niveau der Neuvertragsmieten und der Bestandsmiete
	{
	#	
	# Note: Wenn Nachfrageüberhang können Neuvertragsmieten max. 10 Prozent über Mietspiegel/bisherigen Mietnievau liegen
	# Underrent-Situation vernachlässigt, andere Mieten unegfähr mit Kappungsgrenze (bei 15%: 4,8% p.a., bei 20% 6,3% p.a.)
		
		ifelse(market.cap.limit[1] == 0.15, 0.048, 0.063)
	# Loop over holding period: 
	for (i in 2:holding.period) {

			market.rent[i] <- case_when(
				rent.market.situation[i] == "ausgeglichen"~
  		   		 market.rent[i-1] * (1 - market.churn.rate[i]) * (1.0 +	ifelse(market.cap.limit[i] ==0.15, 0.048, 0.063)) +
			 														 market.rent[i-1] * (1.0 + cpi.total[i]) * (market.churn.rate[i]), # Neuvertragsmieten nur über durchschn. Preisniveau
				rent.market.situation[i] == "nachfrageüberhang"~
  		   		 market.rent[i-1] * (1 - market.churn.rate[i]) * (1.0 + ifelse(market.cap.limit[i] ==0.15, 0.048, 0.063)) +
			 														 market.rent[i-1] * 1.1 * (market.churn.rate[i]), # Neuvertragsmieten über möglichen Anstieg über Mietspiegel
				rent.market.situation[i] == "angebotsüberhang"~
  		   		 market.rent[i-1] * (1 - market.churn.rate[i]) * (1.0 + 0.02) +
			 														 market.rent[i-1] *  (1.0 + 0.02) * (market.churn.rate[i]) # real bleiben Mieten auf Niveau, nominal steigen mit CPI
		)
		}
	# calc_cagr(market.rent, 1)
	}			
	
	# 3 Entwicklung Marktleerstand ("market.vacancy.rate") MISSING
	{
	# market.vacancy.rate	- dynamische Modellierung später
	}
	
	# 4 Entwicklung Marktfluktuation ("market.churn.rate")
	{
	# market.churn.rate	- dynamische Modellierung später - 
	}
	
	# 5 Entwicklung Affordability am Gesamtmarkt ("temp.market.affordability.rate")
	# für ähnlich spezifizierte Wohnung (Größenklasse)	
	{
	#	temp.market.affordability.rate 
		for (i in 1:holding.period) {
			temp.market.affordability.rate[i] <- 
					((market.rent[i] + 
					 market.warm.util.cost[i] + 
				   market.cold.util.cost[i]) * unit.size[i]) / 
				((market.household.income[i]) * 
				(ifelse(unit.size[i] < 70, unit.size[i]/35, market.household.size[i])/market.household.size[i])
				)
	}
	
  }	
	}
	
	#-----------------------

	## Step 3: Pre-Calculations: Object Specific Calculations
	{
	## 1) Different object specific indicators
		
	#	1 Specific rental value and expected development of object-specific market rent ("object.market.rent")
	{
	# Bestimmung der objektspezifischen Marktmiete ("Vergleichsmiete") - auch als Mietspiegel, Fortschreibung mit Kaufkraft
	
	## Step 1) Object valuation by hedonic model	t = 1
	# as an input
	
	## Step 1) Check for modernizations
	if(any(is.na(modernization.period == TRUE)) == TRUE){
		valuation.rent.modernization <- object.market.rent
		modernization.add <- rep(0, holding.period)
	} else {
	  modernization.add <- rep(0, holding.period)
	  modernization.add[modernization.period] <- (valuation.rent.modernization[1]/object.market.rent[1])-1
	}
		
	## Step 2) development by market rent plus add-on for opt. change in characteristics (modernization) at time t
	for (i in 2:holding.period) {
	object.market.rent[i] <- object.market.rent[i-1] * 
		(1 + calc_cagr(market.rent, 1)[i] + modernization.add[i])
	}
	# calc_cagr(object.market.rent, 1)
	}
		
	#	2 Specific churn rate ("object.churn.rate")
	{
	# Loop over holding period: same as before 
	for (i in 2:holding.period) {
			object.churn.rate[i] <- object.churn.rate[i-1] * (1.0)
		}	
	}
		
	#	4 Specific vacancy rate ("object.vacancy.rate")
	{
	# Loop over holding period: same as before - 
	for (i in 2:holding.period) {
			object.vacancy.rate[i] <- object.vacancy.rate[i-1] * (1.0)
		}	
	}	
		
	#	6 Specific household income ("object.household.income")
	{
	# Loop over holding period: 
	for (i in 2:holding.period) {
			object.household.income[i] <- object.household.income[i-1] * (1.0 + income.growth[i])
		}	
	}	
	
	#	7 Specific warm and cold utility cost ("object.warm.util.cost"/"object.cold.util.cost")
	{
	# Note: die objektspezifische Nebenkosten entweder als Input oder Approx. über Marktdaten,
		# die Fortschreibung erfolgt dabei über die erwartete Marktentwicklung. Mögliche Änderungen der
		# Objektmerkmale (re Modernisierung) werden als Veränderung der approx. Marktdaten (neue vs. alte Merkmale) berücksichtigt
		
	# Step 1) Set initial costs	(input-given or approx.)	
	if(object.cold.util.cost[1] == 0){
	  object.cold.util.cost <- UtilityCosts()[1] # wenn keine objektspezifischen Kosten angegeben sind, wird approximiert
	  object.warm.util.cost <- UtilityCosts()[2] # wenn keine objektspezifischen Kosten angegeben sind, wird approximiert
	}

	# Step 2.1 ) initialize the vector: by market approx utility costs
	app.object.cold.util.cost <- rep(0, holding.period)
	app.object.warm.util.cost <- rep(0, holding.period)
	# calc. initial market approx. util. cost
	app.object.cold.util.cost[1] <- UtilityCosts()[1]
	app.object.warm.util.cost[1] <- UtilityCosts()[2]

	# Step 2.2) Loop over the years (approx costs) controlling for changes in object specifics
	for (i in 2:holding.period) {
	# previous costs times the expected inflation and plus the object specific change in characteristics	
	app.object.cold.util.cost[i] <- app.object.cold.util.cost[i-1] * (1 + cpi.utils.cost.cold[i] + (UtilityCosts()[1]/UtilityCosts()[1] - 1))
	app.object.warm.util.cost[i] <- app.object.warm.util.cost[i-1] * (1 + cpi.utils.cost.warm[i] + (UtilityCosts()[2]/UtilityCosts()[2] - 1))
	}
	
	# Step 3) Loop over the years (using the approx cost which controls for general cost
	# increase and possible change in characteristics)
	for (i in 2:holding.period) {
	object.cold.util.cost[i] <- object.cold.util.cost[i-1] * (1 + calc_cagr(app.object.cold.util.cost, 1)[i])
	object.warm.util.cost[i] <- object.warm.util.cost[i-1] * (1 + calc_cagr(app.object.warm.util.cost, 1)[i])
	}
		
	}			
	
	#	8 Specific affordability rate (market rent for object) ("temp.object.affordability.rate")
	{
	#	temp.object.affordability.rate 
		for (i in 1:holding.period) {
			temp.object.affordability.rate[i] <- 
					((object.market.rent[i] + 
					 object.warm.util.cost[i] + 
				   object.cold.util.cost[i]) * unit.size[i]) / 
				(object.household.income[i] * 
				(ifelse(unit.size[i] < 70, unit.size[i]/35, market.household.size[i])/market.household.size[i])
				)
		}
		
		
		
	}		
		
	# 9 Specific mietspiegel rent dev with fixed rate
	{
	## Note
	#	
	if(is.na(object.mietspiegel.rent[1]) == FALSE)
	{
	object.mietspiegel.rent <- mietspiegel.rent
	
	## Wenn Mietspiegelwert vorliegt, dann Steigerung mit CPI und ggf. Modernisierungszuschlag
	for (i in 2:holding.period) {
	object.mietspiegel.rent[i] <- object.mietspiegel.rent[i-1] * 
		(1 + cpi.total[i] + modernization.add[i])
	}
	
	## Wenn kein Mietspiegelwert vorliegt
	} else {
 object.mietspiegel.rent <- object.market.rent
	}
	
	}
	
	# 10 Specific CO2-Cost and split MISSING
	{		
	}
		
	}	
		
	#-----------------------

	# Step 4: Income Workings
	{
	## Content
	{	
	# Desc.: 	
		# Basierend auf IST-Miete, Entwicklung begrenzt durch Marktgängigkeit, Erschwinglichkeit,
		# Entwicklung Markt: Angebots-Nachfrage-Verhältnis, Kappungsgrenze, Fluktuation, 
		# Mögliche Entwicklung des Objekts (revenue-generating capex) und Modernisierungsaufschlag
		# Es wird die Entwicklung der Vetragsmieten als auch die Neuvertragsmieten separat ermittelt
	}		

	### Step 2.1) Rental Income Workings (Methodik als Graphic veranschaulichen)
	{
		
		## I) Temp Initials
		{
		# Gesamtmiete	zum Bewertungszeitpunkt
		total.rent.sqm[1] <- in.place.rent.sqm[1]
		total.rent.sqm.B[1] <- in.place.rent.sqm.B[1]
		total.rent.sqm.C[1] <- in.place.rent.sqm.C[1]

		# Neuvertragsmiete zum Bewertungszeitpunkt
		new.rent.sqm[1] <- in.place.rent.sqm[1]
		new.rent.sqm.B[1] <- in.place.rent.sqm.B[1]
		new.rent.sqm.C[1] <- in.place.rent.sqm.C[1]

		# Belastungsmiete zum Bewertungszeitpunkt
		temp.set.market.affordability.rate <- temp.market.affordability.rate
		temp.in.place.afford.rent.sqm[1] <- ((temp.set.market.affordability.rate[1] * object.household.income[1]) / unit.size[1]) - object.cold.util.cost[1] - object.warm.util.cost[1]

		}
		
		## II) Run-through: Bestandsverträge
		for (i in 2:holding.period) {
		 	
		 	## Bestandsmietverträge (Option A, B, C)
		 	
		 	# Step Option 1: Modernisierung
		 	{
		 	## Beschreibung: Mögliche Modernisierung berücksichtigen	
		 	## Option A:	
		 	if((modernization.periods[i] == 1) == TRUE){
		 		# Max. Modernisierungsumlage (abzgl. vergangener Umlagen)
		 		Mod.umlage[i] <- modernization.cost[i] * 0.08 - sum(Mod.umlage[0:i])

		 		# Wenn unter 6 Euro Ist-Miete Umlage max 2 Euro, sonst max 3 Euro
		 		Mod.umlage[i] <- case_when(
		 			in.place.rent.sqm[i-1] <= 6 ~ max(Mod.umlage[i], 2),
		 			in.place.rent.sqm[i-1] > 6 ~ max(Mod.umlage[i], 3)
		 		)
		 		
		 		# Wert auf altes Niveau aufschlagen
		 		in.place.rent.sqm[i] <- in.place.rent.sqm[i-1] + Mod.umlage[i]
		 		in.place.mod.rent.growth[i] <- (in.place.rent.sqm[i]/in.place.rent.sqm[i-1] - 1)

		 		# 
		 		# keine Modernisierung, Mietniveau auf altem Niveau halten
		 	} else {
		 		in.place.rent.sqm[i] <- in.place.rent.sqm[i-1]
		 		in.place.mod.rent.growth[i] <- 0
		 	}
		 		
		 ## Option B:	
		 	if((modernization.periods[i] == 1) == TRUE){
		 		# Max. Modernisierungsumlage (abzgl. vergangener Umlagen)
		 		Mod.umlage[i] <- modernization.cost[i] * 0.08 - sum(Mod.umlage[0:i])

		 		# Wenn unter 6 Euro Ist-Miete Umlage max 2 Euro, sonst max 3 Euro
		 		Mod.umlage[i] <- case_when(
		 			in.place.rent.sqm.B[i-1] <= 6 ~ max(Mod.umlage[i], 2),
		 			in.place.rent.sqm.B[i-1] > 6 ~ max(Mod.umlage[i], 3)
		 		)
		 		
		 		# Wert auf altes Niveau aufschlagen
		 		in.place.rent.sqm.B[i] <- in.place.rent.sqm.B[i-1] + Mod.umlage[i]
		 		in.place.mod.rent.growth.B[i] <- (in.place.rent.sqm.B[i]/in.place.rent.sqm.B[i-1] - 1)

		 		# 
		 		# keine Modernisierung, Mietniveau auf altem Niveau halten
		 	} else {
		 		in.place.rent.sqm.B[i] <- in.place.rent.sqm.B[i-1]
		 		in.place.mod.rent.growth.B[i] <- 0
		 	}
		 		
		 ## Option C:	
		 	if((modernization.periods[i] == 1) == TRUE){
		 		# Max. Modernisierungsumlage (abzgl. vergangener Umlagen)
		 		Mod.umlage[i] <- modernization.cost[i] * 0.08 - sum(Mod.umlage[0:i])

		 		# Wenn unter 6 Euro Ist-Miete Umlage max 2 Euro, sonst max 3 Euro
		 		Mod.umlage[i] <- case_when(
		 			in.place.rent.sqm.C[i-1] <= 6 ~ max(Mod.umlage[i], 2),
		 			in.place.rent.sqm.C[i-1] > 6 ~ max(Mod.umlage[i], 3)
		 		)
		 		
		 		# Wert auf altes Niveau aufschlagen
		 		in.place.rent.sqm.C[i] <- in.place.rent.sqm.C[i-1] + Mod.umlage[i]
		 		in.place.mod.rent.growth.C[i] <- (in.place.rent.sqm.C[i]/in.place.rent.sqm.C[i-1] - 1)

		 		# 
		 		# keine Modernisierung, Mietniveau auf altem Niveau halten
		 	} else {
		 		in.place.rent.sqm.C[i] <- in.place.rent.sqm.C[i-1]
		 		in.place.mod.rent.growth.C[i] <- 0
		 	}
		 		
		 	}
		 	
		 	# Step Option 2: Belastungsmiete (kalt)
		 	{
		 	## Beschreibung: Belastungsmiete (kalt) bestimmen, wenn Wert kleiner als aktueller auf Vorjahresniveau bleiben
		 	
		 	## anzusetzende Belastungsquote bestimmen	
		 	temp.set.market.affordability.rate[i] <- case_when(
		 		
		 	# wenn ausgeglichen Belastungsquote halten
			rent.market.situation[i] == "ausgeglichen"~
			 														 temp.set.market.affordability.rate[i-1],
		  # wenn Nachfrageüberhang Belastungsquote steigt, aber nicht über kritische Schwelle (Markt +5%)
			rent.market.situation[i] == "nachfrageüberhang"~
			 														 ifelse(temp.set.market.affordability.rate[1] + 0.05  >= temp.set.market.affordability.rate[i-1], temp.set.market.affordability.rate[i-1] + 0.005, temp.market.affordability.rate[i-1]),
			# wenn Angebotsüberhang Belastungsquote halten
		 	rent.market.situation[i] == "angebotsüberhang"~
			 														 temp.set.market.affordability.rate[i-1]
			)
		 		
		 	# Option A
		 	## Belastungsmiete (kalt) bestimmen
		 	temp.in.place.afford.rent.sqm[i] <- ((temp.set.market.affordability.rate[i] * object.household.income[i]) / unit.size[i]) - object.cold.util.cost[i] - object.warm.util.cost[i]
				
		 	## Geplante Miete setzen je nachdem was höher ist
			in.place.rent.sqm[i] <- case_when(
		 			temp.in.place.afford.rent.sqm[i] > in.place.rent.sqm[i] ~ temp.in.place.afford.rent.sqm[i],
		 			temp.in.place.afford.rent.sqm[i] <= in.place.rent.sqm[i] ~ in.place.rent.sqm[i]
		 		)
		 	
		 	# Option B (ohne Rücksicht auf Belastungsmiete)
		 	in.place.rent.sqm.B[i] <- in.place.rent.sqm.B[i-1]
		 	
		 	# Option C
		  ## Geplante Miete setzen je nachdem was höher ist
			in.place.rent.sqm.C[i] <- case_when(
		 			temp.in.place.afford.rent.sqm[i] > in.place.rent.sqm.C[i] ~ temp.in.place.afford.rent.sqm[i],
		 			temp.in.place.afford.rent.sqm[i] <= in.place.rent.sqm.C[i] ~ in.place.rent.sqm.C[i]
		 		)
		 	
		 	}
		 		
		 	# Step 3: Mietspiegel / Marktmietvergleich
		 	{
		 	##	Vergleich zur "Markt"-Miete bzw. Mietspiegel, wenn Mietpreisbremse gilt (object.market.rent kann "Marktmiete" oder "Mietspiegelmiete" sein)
		 	#   Überprüfen, dass der neue Wert nicht über Mietspiegel bzw. Marktmietnievau läuft	
		 	
		 	## Option A
		 	## wenn der Wert (aus Step 2) den kritischen Wert "Marktmiete + 10 %" übersteigt auf letzteren als max Wert setzen
			in.place.rent.sqm[i] <- case_when(
		 			in.place.rent.sqm[i] >= object.market.rent[i] * 1.1 ~ object.market.rent[i] * 1.1,
		 			in.place.rent.sqm[i] < object.market.rent[i] * 1.1 ~ in.place.rent.sqm[i]
		 		)
		 			 		
		 	## Option B
 			## Miete auf kritischen Wert setzen "Marktmiete + 10 %" 
			in.place.rent.sqm.B[i] <- object.market.rent[i] * 1.1
			
			## Option C
		 	## wenn der Wert (aus Step 2) den kritischen Wert "Marktmiete + 10 %" übersteigt auf letzteren max Wert setzen
			in.place.rent.sqm.C[i] <- case_when(
		 			in.place.rent.sqm.C[i] >= object.market.rent[i] * 1.1 ~ object.market.rent[i] * 1.1,
		 			in.place.rent.sqm.C[i] < object.market.rent[i] * 1.1 ~ in.place.rent.sqm.C[i]
		 	)
			}
		 	
			## Hier: Staffel und Index als optionen 
			
		 	# Step 4: Kappungsgrenzen (BGB) prüfen
		 	{
		 	## wenn der Wert (abzgl. Mod-Anstieg) die Kappungsgrenze übersteigt, auf max. Erhöhung nach BGB setzen
		 	## Annahme: max. Erhöhung verteilt sich gleichmäßig auf die 3 Jahre
		 		
		 	## Option A:	
			in.place.rent.sqm[i]  <- case_when(
		 			(in.place.rent.sqm[i]/in.place.rent.sqm[i-1] - 1) - Mod.umlage[i] <= market.cap.limit[i]/3 ~ 
					in.place.rent.sqm[i],
				##
		 			(in.place.rent.sqm[i]/in.place.rent.sqm[i-1] - 1) - Mod.umlage[i] > market.cap.limit[i]/3 ~ 
					in.place.rent.sqm[i-1]  * (1 + Mod.umlage[1] + market.cap.limit[i]/3)
		 		)
		 		
		 	## Option B:
		 	in.place.rent.sqm.B[i] <- case_when(
		 			(in.place.rent.sqm.B[i]/in.place.rent.sqm.B[i-1] - 1) - Mod.umlage[i] <= market.cap.limit[i]/3 ~ 
					in.place.rent.sqm.B[i],
				##
		 			(in.place.rent.sqm.B[i]/in.place.rent.sqm.B[i-1] - 1) - Mod.umlage[i] > market.cap.limit[i]/3 ~ 
					in.place.rent.sqm.B[i-1]  * (1 + Mod.umlage[1] + market.cap.limit[i]/3)
		 		)
		 	
		 	## Option C:
		 	in.place.rent.sqm.C[i] <- case_when(
		 			(in.place.rent.sqm.C[i]/in.place.rent.sqm.C[i-1] - 1) - Mod.umlage[i] <= cpi.total[i] ~ 
					in.place.rent.sqm.C[i],
				##
		 			(in.place.rent.sqm.C[i]/in.place.rent.sqm.C[i-1] - 1) - Mod.umlage[i] > cpi.total[i] ~ 
					in.place.rent.sqm.C[i-1]  * (1 + Mod.umlage[1] + cpi.total[i])
		 		)	
		 		
		 	}
		 		
		 	# mean(calc_cagr(in.place.rent.sqm, 1)[2:holding.period])
			# mean(calc_cagr(in.place.rent.sqm.B, 1)[2:holding.period])

		 }
		
		## III) Run-through: Neuvermietungen
		for (i in 2:holding.period) {	
		 	## Beschreibung:
  		# Anpassung des Mietniveaus über Fluktuation (abhängig von der Durchsetzbarkeit am Markt wird die Neuvertragsmiete gesetzt)
			# Für jedes Jahr wird theoretisch der Wert angegeben, auf den man am Markt die Miete gegeben der Marktsituaiton setzen könnte
			# Aber maximal +10% über Mietspiegel/Marktmietniveau
			
			## Option A: 
		  new.rent.sqm[i] <- case_when(
		 	
		 	## wenn ausgeglichen Miete mindestens auf Mietspiegelniveau (wenn nicht schon höher)
			rent.market.situation[i] == "ausgeglichen"~
			 														 ifelse(object.mietspiegel.rent[i]*1.1 <= in.place.rent.sqm[i], in.place.rent.sqm[i], object.mietspiegel.rent[i]),
		  ## wenn Nachfrageüberhang Miete mindestens auf Mietspiegelniveau +10% (wenn nicht schon höher)
			rent.market.situation[i] == "nachfrageüberhang"~
			 														 ifelse(object.mietspiegel.rent[i]*1.1 <= in.place.rent.sqm[i], in.place.rent.sqm[i], object.mietspiegel.rent[i])  * (1 + 0.1),
			## wenn Angebotsüberhang Miete mindestens auf Altniveau + Inflation% (wenn nicht schon höher)
		 	rent.market.situation[i] == "angebotsüberhang"~
			 														 ifelse(object.mietspiegel.rent[i]*1.1 <= in.place.rent.sqm[i], in.place.rent.sqm[i], in.place.rent.sqm[i])  * (1 + cpi.total[i]),
			)
		 	
		  ## Option B: 
		  new.rent.sqm.B[i] <- case_when(
		 	
		 	## wenn ausgeglichen Miete mindestens auf Mietspiegelniveau (wenn nicht schon höher)
			rent.market.situation[i] == "ausgeglichen"~
			 														 ifelse(object.mietspiegel.rent[i]*1.1 <= in.place.rent.sqm.B[i], in.place.rent.sqm.B[i], object.mietspiegel.rent[i]),
		  ## wenn Nachfrageüberhang Miete mindestens auf Mietspiegelniveau +10% (wenn nicht schon höher)
			rent.market.situation[i] == "nachfrageüberhang"~
			 														 ifelse(object.mietspiegel.rent[i]*1.1 <= in.place.rent.sqm.B[i], in.place.rent.sqm.B[i], object.mietspiegel.rent[i])  * (1 + 0.1),
			## wenn Angebotsüberhang Miete mindestens auf Altniveau + Inflation% (wenn nicht schon höher)
		 	rent.market.situation[i] == "angebotsüberhang"~
			 														 ifelse(object.mietspiegel.rent[i]*1.1 <= in.place.rent.sqm.B[i], in.place.rent.sqm.B[i], in.place.rent.sqm.B[i])  * (1 + cpi.total[i]),
			)
			
		  ## Option C: 
		  new.rent.sqm.C[i] <- case_when(
		 	
		 	## wenn ausgeglichen Miete mindestens auf Mietspiegelniveau (wenn nicht schon höher)
			rent.market.situation[i] == "ausgeglichen"~
			 														 ifelse(object.mietspiegel.rent[i]*1.1 <= in.place.rent.sqm.C[i], in.place.rent.sqm.C[i], object.mietspiegel.rent[i]),
		  ## wenn Nachfrageüberhang Miete mindestens auf Mietspiegelniveau +10% (wenn nicht schon höher)
			rent.market.situation[i] == "nachfrageüberhang"~
			 														 ifelse(object.mietspiegel.rent[i]*1.1 <= in.place.rent.sqm.C[i], in.place.rent.sqm.C[i], object.mietspiegel.rent[i])  * (1 + 0.1),
			## wenn Angebotsüberhang Miete mindestens auf Altniveau + Inflation% (wenn nicht schon höher)
		 	rent.market.situation[i] == "angebotsüberhang"~
			 														 ifelse(object.mietspiegel.rent[i]*1.1 <= in.place.rent.sqm.C[i], in.place.rent.sqm.C[i], in.place.rent.sqm.C[i])  * (1 + cpi.total[i]),
			)
		 # mean(calc_cagr(new.rent.sqm, 1)[2:holding.period])
		}
		
		## IV) Run-through: ERV (simulierte Mietentwicklung)
		{
		## Savings data (for data savings return)
		Temp.data.list <- list()
			
		## Durchläufe diverser Simulationen
		for(k in 1:sim.n){	
			
		## IV) Run-through: Gesamt
		for (i in 2:holding.period) {	
  		## Total rent: Mischung aus Bestands und Neuvertragsmieten: (Option A und B)
		 	# Annahmen: Wahrscheinlichkeit des Auszugs gleich für Bestand und Total (die die zuvor gewechselt sind)
			# Annahmen: der Anteil der Mieter, die zuvor gewechselt sind (Marktniveau), dort steigt die Miete nur um CPI
  		
			### Option A)
			{
			tenant.shift[i] <- rbinom(1, 1, object.churn.rate)

			## Case 1: Kein Mieterwechsel (-> dann entwickelte Bestandsmiete)
			if(tenant.shift[i] == 0 & any(tenant.shift[2:(i-1)] == 1) == FALSE){
			total.rent.sqm[i] <- in.place.rent.sqm[i]
			
			## Case 2: Mieterwechsel (-> dann entwickelte Marktmiete)	
			} else if(tenant.shift[i] == 1){
			total.rent.sqm[i] <- new.rent.sqm[i]
			
			## Case 3: Kürzlicher Mieterwechsel (-> dann Marktmiete nur über CPI fortschreiben)
			} else if(tenant.shift[i] == 0 & any(tenant.shift[2:(i-1)] == 1) == TRUE){

			total.rent.sqm[i] <- total.rent.sqm[i-1] * (1 +  (object.mietspiegel.rent[i]/object.mietspiegel.rent[i-1] - 1))

			}
			
			}
			
			### Option B):
			{
			## Kein Mieterwechsel (Entwickelte Bestandsmiete)
			if(tenant.shift[i] == 0 & any(tenant.shift[2:(i-1)] == 1) == FALSE){
			total.rent.sqm.B[i] <- in.place.rent.sqm.B[i]
			
			## Mieterwechsel (Entwickelte Marktmiete)	
			} else if(tenant.shift[i] == 1){
			
			total.rent.sqm.B[i] <- new.rent.sqm.B[i]
			
			## Kürzlicher Mieterwechsel (Marktmiete einmalig nur über CPI fortschreiben)
			} else if(tenant.shift[i] == 0 & any(tenant.shift[2:(i-1)] == 1) == TRUE){

			total.rent.sqm.B[i] <- total.rent.sqm.B[i-1] * (1 + (object.mietspiegel.rent[i]/object.mietspiegel.rent[i-1] - 1))

			}
			
			}
  		
			### Option C):
			{
			## Kein Mieterwechsel (Entwickelte Bestandsmiete)
			if(tenant.shift[i] == 0 & any(tenant.shift[2:(i-1)] == 1) == FALSE){
			total.rent.sqm.C[i] <- in.place.rent.sqm.C[i]
			
			## Mieterwechsel (Entwickelte Marktmiete)	
			} else if(tenant.shift[i] == 1){
			
			total.rent.sqm.C[i] <- new.rent.sqm.C[i]
			
			## Kürzlicher Mieterwechsel (Marktmiete einmalig nur über CPI fortschreiben)
			} else if(tenant.shift[i] == 0 & any(tenant.shift[2:(i-1)] == 1) == TRUE){

			total.rent.sqm.C[i] <- total.rent.sqm.C[i-1] * (1 + (object.mietspiegel.rent[i]/object.mietspiegel.rent[i-1] - 1))

			}
			
			}
			
			## Temp savings
			{
			Temp.data <- data.frame(
												tenant.shift,
												# Total Rents		
												total.rent.sqm,
												total.rent.sqm.B,
												total.rent.sqm.C
											)
		
Temp.data.list[[k]] <- Temp.data
			}
			
		}
		}	
			
		## V) Mean values and confidence interval
		{
			
		# SD und margin für Option A	
		total.rent.sqm.sd <- total.rent.sqm	
		total.rent.sqm.margin <- total.rent.sqm.sd
			
		## Option A:	
		for(h in 1:holding.period){
		# mean 
		total.rent.sqm[h] <- mean(sapply(c(1:sim.n), function(x) (Temp.data.list[[x]][h, 2])))
		# standard dev.
		total.rent.sqm.sd[h] <- sd(sapply(c(1:sim.n), function(x) (Temp.data.list[[x]][h, 2])))
		# margin (5% alpha)
		total.rent.sqm.margin[h]  <- qt(0.975,df=sim.n -1)*total.rent.sqm.sd[h]/sqrt(sim.n)
		}
		
		# SD und margin für Option B
		total.rent.sqm.B.sd <- total.rent.sqm.B
		total.rent.sqm.B.margin <- total.rent.sqm.B.sd
		
		## Option B:	
		for(h in 1:holding.period){
		# mean 
		total.rent.sqm.B[h] <- mean(sapply(c(1:sim.n), function(x) (Temp.data.list[[x]][h, 3])))
		# standard dev.
		total.rent.sqm.B.sd[h] <- sd(sapply(c(1:sim.n), function(x) (Temp.data.list[[x]][h, 3])))
		# margin (5% alpha)
		total.rent.sqm.B.margin[h]  <- qt(0.975,df=sim.n -1)*total.rent.sqm.B.sd[h]/sqrt(sim.n)
		}
		
		# SD und margin für Option C
		total.rent.sqm.C.sd <- total.rent.sqm.C
		total.rent.sqm.C.margin <- total.rent.sqm.C.sd
		
		## Option B:	
		for(h in 1:holding.period){
		# mean 
		total.rent.sqm.C[h] <- mean(sapply(c(1:sim.n), function(x) (Temp.data.list[[x]][h, 4])))
		# standard dev.
		total.rent.sqm.C.sd[h] <- sd(sapply(c(1:sim.n), function(x) (Temp.data.list[[x]][h, 4])))
		# margin (5% alpha)
		total.rent.sqm.C.margin[h]  <- qt(0.975,df=sim.n -1)*total.rent.sqm.C.sd[h]/sqrt(sim.n)
		}
		
		}

			
		}
			
	}

	### Step 2.2) Return key parameters
	{
return(
	data.frame(
	# Markt- und Mietspiegelmiete
	Objekt.Markmiete = round(object.market.rent, 2), # Miete nach Val. und Entwicklung wie Markt
	Objekt.Mietspiegel = round(object.mietspiegel.rent, 2), # Mietspiegel-Miete (wenn keine Angaben = Markt)
	
	# Entwicklung Belastungsmiete	und Belastungsquote
	Belastungsmiete = round(temp.in.place.afford.rent.sqm, 2), # Miete rein über Zahlungsfähigkeit abgeleitet
	Belastungsquote = round(temp.set.market.affordability.rate, 2), # Bruttowarmmietbelastung am Markt

	# Einkommensentwicklung Markt und Mikrolage
		
	# Erwartete Inflation
		
	# Entwicklung Bestandsmieten
	Bestandsmiete.Option.A = round(in.place.rent.sqm, 2), # Option A: Mit Belastungsmiete und Regulatorik
	Bestandsmiete.Option.B = round(in.place.rent.sqm.B, 2), # Option B: Ohne Belastungsmiete aber mit Regulatorik
	Bestandsmiete.Option.C = round(in.place.rent.sqm.C, 2), # Option C: mit Belastungsmiete und nur nach CPI

	# Neuvertragsmiete (bei Mieterwechsel)
	Neuvertragsmiete.Option.A = round(new.rent.sqm, 2), # Option A: Mit Belastungsmiete und Regulatorik
	Neuvertragsmiete.Option.B = round(new.rent.sqm.B, 2), # Option B: Ohne Belastungsmiete aber mit Regulatorik
	Neuvertragsmiete.Option.C = round(new.rent.sqm.C, 2), # Option C: mit Belastungsmiete und nur nach CPI

	# Total Rents	(Kombination aus Bestands- und Neuvertragsmietentwicklung) als simulierte Rechengröße
	ERV.Option.A = round(total.rent.sqm, 2), # Option A: Mit Belastungsmiete und Regulatorik
	ERV.Option.margin.A = round(total.rent.sqm.margin, 2), # 5%-margin, da Miete über n Läufe simuliert
	ERV.Option.B = round(total.rent.sqm.B, 2), # Option B: Ohne Belastungsmiete aber mit Regulatorik
	ERV.Option.margin.B = round(total.rent.sqm.B.margin, 2), # 5%-margin, da Miete über n Läufe simuliert
	ERV.Option.C = round(total.rent.sqm.C, 2), # Option C: mit Belastungsmiete und nur nach CPI
	ERV.Option.margin.C = round(total.rent.sqm.C.margin, 2) # 5%-margin, da Miete über n Läufe simuliert
	)
)	
	}
	}
}
	
	
ERV.Calculation(property.no = 1, unit.no = 1)

}
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
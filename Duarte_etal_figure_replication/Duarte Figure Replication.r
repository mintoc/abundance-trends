############################################################################################################
############################################################################################################
############################################################################################################
#########
#########	Code written by Daniel Hively
#########
#########	This code attempts to replicate Figure 4a from the paper "Rebuilding marine life"
#########	by Duarte et al from Nature in April 1st 2020. It covers several different data
#########	scenarios (based on the available methods provided in the paper and correspondence
#########	with the authors) that were used in attempting to generate the figure.
#########
#########	This code was built using R version 3.6.3, and uses the libraries "stringr" and "spatstat".
#########
#########	This code uses the included input files 'dstocks (v4.41).csv' and 'replicate.scenarios.csv'.
#########	The 'dstocks (v4.41).csv' file provides the subset of stocks listed in the Duarte paper. The 
#########	'replicate.scenarios.csv' file lists the scenarios and their associated data choices used 
#########	to generate the output files.
#########
#########	The Rdata input file for the 'asmt' version of RAMLDB v4.41 is located at the following URL:
#########		https://zenodo.org/record/2542927
#########	The Rdata input file for the 'mdl' version of RAMLDB v4.41 is located at the following URL:
#########		https://zenodo.org/record/3879051
#########	These R data files provide data input for the two RAMLDB versions used in these scenarios.
#########
#########	Folder structure assumed in code:
#########	C:\\figurerep\\
#########	C:\\figurerep\\datload\\
#########	C:\\figurerep\\datload\\v4.41\\
#########	C:\\figurerep\\datload\\v4.41\\asmt\\
#########	C:\\figurerep\\datload\\v4.41\\mdl\\
#########	C:\\figurerep\\scenarios\\
#########
#########	File structure of inputs assumed in code:
#########	C:\\figurerep\\datload\\dstocks (v4.41).csv
#########	C:\\figurerep\\datload\\replicate.scenarios.csv
#########	C:\\figurerep\\datload\\v4.41\\asmt\\DBdata.RData
#########	C:\\figurerep\\datload\\v4.41\\mdl\\DBdata.RData
#########
#########	Instructions:
#########	Select all code and run. The code will iterate through the scenarios and output
#########	summary files in the 'C:\\figurerep\\scenarios\\' folder for each scenario. These
#########	files contain the weighted B/Bmsy for that scenario, counts of stocks used in each
#########	year, and the data choices used for that scenario.
#########



# Package loading and option setting
library(stringr)
library(spatstat)
options(warn=2)
options(digits=15)

## get the main directory which should be the folder in which the code "Duarte Figure Replication.r" is
main_dir <- getwd()

# Load in scenario list
setwd("./datload")
scenarios=t(t(read.csv("replicate.scenarios.csv",header=TRUE)))
SR=dim(scenarios)[1]

setwd(main_dir)

# Loop through scenarios
for (s in 1:SR) {

# Clear data in case new data set is used for new scenario
saveds=c("scenarios","SR","s","main_dir")
rm(list=setdiff(ls(), saveds)) 

# Scenario options
scen.scen=scenarios[s,"Scenario"]
scen.version=scenarios[s,"RAMversion"]
scen.type=scenarios[s,"RAMtype"]
scen.stocks=scenarios[s,"Stocks"]
scen.years=scenarios[s,"Years"]
scen.prefs=scenarios[s,"Prefs"]
scen.weights=scenarios[s,"Weighting"]
scen.agg=scenarios[s,"Aggregate"]

if (scen.type=="asmt-0.5maxB") {dsource="asmt"}
if (scen.type=="asmt") {dsource="asmt"}
if (scen.type=="mdl") {dsource="mdl"}

########################################################################################################
########################################################################################################
########################################################################################################
########################################################################################################
########################################################################################################
########################################################################################################
########################################################################################################
########################################################################################################
########################################################################################################
########################################################################################################
###################################                        #############################################
###################################      Data Importing    #############################################
###################################                        #############################################
########################################################################################################
########################################################################################################
########################################################################################################
########################################################################################################
########################################################################################################
########################################################################################################
########################################################################################################
########################################################################################################
########################################################################################################
########################################################################################################
########################################################################################################
###################
###################	Data is loaded based on the scenario version, scenario type and 
###################	stock subsetting.
###################



##########################
### v4.41 (Full)
##########################

if (scen.version=="v4.41" && scen.stocks=="all") {
	if (dsource=="asmt") {
		setwd("./datload/v4.41/asmt/")
		load("DBdata.RData")
	}
	if (dsource=="mdl") {
		setwd("./datload/v4.41/mdl/")
		load("DBdata.RData")
	}
        setwd(main_dir)
        
	c999.ptr=which(999==as.numeric(assessment[,"mostrecent"]))
	fdat=assessment[c999.ptr,c("assessid","stockid","stocklong")]
	vers=scen.version

	TS=timeseries
	BP=bioparams
	TSV=timeseries_values_views
	BPV=bioparams_values_views
	TSI=timeseries_ids_views
	BPI=bioparams_ids_views

	NT=dim(fdat)[1]
	MT=dim(assessment)[1]
	ST=dim(stock)[1]
	duarte.eyear=2016
}



##########################
### v4.41 (Duarte subset)
##########################

if (scen.version=="v4.41" && scen.stocks=="Duarte") {
	if (dsource=="asmt") {
		setwd("./datload/v4.41/asmt/")
		load("DBdata.RData")
	}
        setwd(main_dir)
	setwd("./datload/")
	dstocks=t(t(read.csv("dstocks (v4.41).csv",header=TRUE)))

	DRT=dim(dstocks)[1]
	vers=scen.version

	fdat=matrix("",nrow=DRT,ncol=3)
	colnames(fdat)=c("assessid","stockid","stocklong")
	for (i in 1:DRT) {
		stockid=dstocks[i,"dstocks"]
		aptr=which((stockid==assessment[,"stockid"]) & (999==as.numeric(assessment[,"mostrecent"])))
		if (length(aptr)>0) {
			fdat[i,]=c(assessment[aptr,"assessid"],stockid,assessment[aptr,"stocklong"])
		}
	}

	TS=timeseries
	BP=bioparams
	TSV=timeseries_values_views
	BPV=bioparams_values_views
	TSI=timeseries_ids_views
	BPI=bioparams_ids_views

	NT=dim(fdat)[1]
	MT=dim(assessment)[1]
	ST=dim(stock)[1]
	duarte.eyear=2016
}






########################################################################################################
########################################################################################################
########################################################################################################
########################################################################################################
########################################################################################################
########################################################################################################
########################################################################################################
########################################################################################################
########################################################################################################
########################################################################################################
###################################                        #############################################
###################################    Data Formating      #############################################
###################################                        #############################################
########################################################################################################
########################################################################################################
########################################################################################################
########################################################################################################
########################################################################################################
########################################################################################################
########################################################################################################
########################################################################################################
########################################################################################################
########################################################################################################
########################################################################################################
###################
###################	Data is formatted based on the data preferences, scenario weighting, years,
###################	and scenario type.
###################



##########################
### Prefs data (and years) (not equal weighting) (no 0.5maxB as Bmsy proxy)
##########################

if (scen.prefs=="allTBandSSB" && scen.weights!="equal" && scen.type!="asmt-0.5maxB") {
	dchecker=numeric(NT)
	year.min=2020
	year.max=0
	vids=matrix("",ncol=3,nrow=NT)
	colnames(vids)=c("stockid","data","weight")

	for (i in 1:NT) {
		stockid=fdat[i,"stockid"]
		ids.ptr=which(stockid==TSI[,"stockid"])
		vals.ptr=which(stockid==TSV[,"stockid"])
		years.ptr=which(stockid==timeseries_years_views[,"stockid"])
		vids[i,"stockid"]=stockid

		if (length(ids.ptr)>0) {
			dch=""
			ich=""
			if ((dch=="") && (!is.na(TSI[ids.ptr,"BdivBmsypref"]))) {dch="BdivBmsypref"}

			if ((ich=="") && (!is.na(TSI[ids.ptr,"TB"]))) {
				if (TSI[ids.ptr,"TB"]=="TB-MT") {ich="TB"}
			}
			if ((ich=="") && (!is.na(TSI[ids.ptr,"SSB"]))) {
				if (TSI[ids.ptr,"SSB"]=="SSB-MT") {ich="SSB"}
			}

			if (dch!="" && ich!="") {
				dchecker[i]=1
				yr.rng1=timeseries_years_views[years.ptr,dch]
				yr.str1=unlist(strsplit(yr.rng1,"-"))
				yr.min1=as.numeric(yr.str1[1])
				yr.max1=as.numeric(yr.str1[2])

				yr.rng2=timeseries_years_views[years.ptr,ich]
				yr.str2=unlist(strsplit(yr.rng2,"-"))
				yr.min2=as.numeric(yr.str2[1])
				yr.max2=as.numeric(yr.str2[2])

				yr.min=max(yr.min1,yr.min2)
				yr.max=min(yr.max1,yr.max2)
				if (year.min>yr.min) {year.min=yr.min}
				if (year.max<yr.max) {year.max=yr.max}
				vids[i,"data"]=dch
				vids[i,"weight"]=ich		
			}
		}
	}

	years=year.min:year.max
	if (scen.years=="paper") {years=1980:duarte.eyear}
	YT=length(years)

	vals.data=matrix(NA,ncol=length(years),nrow=NT)
	colnames(vals.data)=years
	vals.weight=matrix(NA,ncol=length(years),nrow=NT)
	colnames(vals.weight)=years

	for (i in 1:NT) {
	if (dchecker[i]==1) {
		stockid=fdat[i,"stockid"]
		tsv.ptr=which(stockid==TSV[,"stockid"])
		ts.data=TSV[tsv.ptr,vids[i,"data"]]
		ts.weight=TSV[tsv.ptr,vids[i,"weight"]]
		ts.year=TSV[tsv.ptr,"year"]

		ts.data.nna.ptr=which(!is.na(ts.data))
		ts.data.use=ts.data[min(ts.data.nna.ptr):max(ts.data.nna.ptr)]
		ts.data.year=ts.year[min(ts.data.nna.ptr):max(ts.data.nna.ptr)]

		ts.weight.nna.ptr=which(!is.na(ts.weight))
		ts.weight.use=ts.weight[min(ts.weight.nna.ptr):max(ts.weight.nna.ptr)]
		ts.weight.year=ts.year[min(ts.weight.nna.ptr):max(ts.weight.nna.ptr)]

		ts.use.year.min=max(min(ts.data.year),min(ts.weight.year))
		ts.use.year.max=min(max(ts.data.year),max(ts.weight.year))
		ts.use.year=ts.use.year.min:ts.use.year.max

		ts.data.use.ptr=which(ts.data.year %in% ts.use.year)
		ts.data.use2=ts.data.use[ts.data.use.ptr]
		ts.data.year2=ts.data.year[ts.data.use.ptr]

		ts.weight.use.ptr=which(ts.weight.year %in% ts.use.year)
		ts.weight.use2=ts.weight.use[ts.weight.use.ptr]
		ts.weight.year2=ts.weight.year[ts.weight.use.ptr]

		data.dtot=which(ts.data.year2 %in% years)
		data.ttod=which(years %in% ts.data.year2)

		weight.dtot=which(ts.weight.year2 %in% years)
		weight.ttod=which(years %in% ts.weight.year2)

		if ((length(data.dtot)>0) && (length(weight.dtot)>0)) {
			vals.data[i,data.ttod]=ts.data.use2[data.dtot]
			vals.weight[i,weight.ttod]=ts.weight.use2[weight.dtot]
		} else {
			dchecker[i]=0
		}
	}
	}
}



##########################
### TB data (and years) (not equal weighting) (0.5maxB as Bmsy proxy possible)
##########################

if (scen.prefs=="onlyTBmsy-mgt" && scen.weights!="equal" && scen.type=="asmt-0.5maxB") {
	dchecker=numeric(NT)
	year.min=2020
	year.max=0
	vids=matrix("",ncol=3,nrow=NT)
	colnames(vids)=c("stockid","data","weight")
	b.check=numeric(NT)

	for (i in 1:NT) {
		stockid=fdat[i,"stockid"]
		ids.ptr=which(stockid==TSI[,"stockid"])
		vals.ptr=which(stockid==TSV[,"stockid"])
		years.ptr=which(stockid==timeseries_years_views[,"stockid"])
		vids[i,"stockid"]=stockid

		if (length(ids.ptr)>0) {
			dch=""
			ich=""

			if ((dch=="") && (!is.na(TSI[ids.ptr,"TBdivTBmsy"]))) {dch="TBdivTBmsy"}
			if ((dch=="") && (!is.na(TSI[ids.ptr,"TBdivTBmgt"]))) {dch="TBdivTBmgt"}
			if ((dch=="") && (!is.na(TSI[ids.ptr,"TB"]))) {
				if (TSI[ids.ptr,"TB"]=="TB-MT") {
					dch="TB"
					b.check[i]=1
				}
			}

			if ((ich=="") && (!is.na(TSI[ids.ptr,"TB"]))) {
				if (TSI[ids.ptr,"TB"]=="TB-MT") {ich="TB"}
			}
			if (dch!="" && ich!="") {
				dchecker[i]=1
				yr.rng1=timeseries_years_views[years.ptr,dch]
				yr.str1=unlist(strsplit(yr.rng1,"-"))
				yr.min1=as.numeric(yr.str1[1])
				yr.max1=as.numeric(yr.str1[2])

				yr.rng2=timeseries_years_views[years.ptr,ich]
				yr.str2=unlist(strsplit(yr.rng2,"-"))
				yr.min2=as.numeric(yr.str2[1])
				yr.max2=as.numeric(yr.str2[2])

				yr.min=max(yr.min1,yr.min2)
				yr.max=min(yr.max1,yr.max2)
				if (year.min>yr.min) {year.min=yr.min}
				if (year.max<yr.max) {year.max=yr.max}
				vids[i,"data"]=dch
				vids[i,"weight"]=ich		
			}
		}
	}

	years=year.min:year.max
	if (scen.years=="paper") {years=1980:duarte.eyear}
	YT=length(years)

	vals.data=matrix(NA,ncol=length(years),nrow=NT)
	colnames(vals.data)=years
	vals.weight=matrix(NA,ncol=length(years),nrow=NT)
	colnames(vals.weight)=years

	for (i in 1:NT) {
	if (dchecker[i]==1) {
		stockid=fdat[i,"stockid"]
		tsv.ptr=which(stockid==TSV[,"stockid"])
		ts.data=as.numeric(TSV[tsv.ptr,vids[i,"data"]])
		ts.weight=TSV[tsv.ptr,vids[i,"weight"]]
		ts.year=TSV[tsv.ptr,"year"]

		ts.data.nna.ptr=which(!is.na(ts.data))
		ts.data.use=ts.data[min(ts.data.nna.ptr):max(ts.data.nna.ptr)]
		ts.data.year=ts.year[min(ts.data.nna.ptr):max(ts.data.nna.ptr)]

		ts.weight.nna.ptr=which(!is.na(ts.weight))
		ts.weight.use=ts.weight[min(ts.weight.nna.ptr):max(ts.weight.nna.ptr)]
		ts.weight.year=ts.year[min(ts.weight.nna.ptr):max(ts.weight.nna.ptr)]

		ts.use.year.min=max(min(ts.data.year),min(ts.weight.year))
		ts.use.year.max=min(max(ts.data.year),max(ts.weight.year))
		ts.use.year=ts.use.year.min:ts.use.year.max

		ts.data.use.ptr=which(ts.data.year %in% ts.use.year)
		ts.data.use2=ts.data.use[ts.data.use.ptr]
		ts.data.year2=ts.data.year[ts.data.use.ptr]

		ts.weight.use.ptr=which(ts.weight.year %in% ts.use.year)
		ts.weight.use2=ts.weight.use[ts.weight.use.ptr]
		ts.weight.year2=ts.weight.year[ts.weight.use.ptr]

		data.dtot=which(ts.data.year2 %in% years)
		data.ttod=which(years %in% ts.data.year2)

		weight.dtot=which(ts.weight.year2 %in% years)
		weight.ttod=which(years %in% ts.weight.year2)

		if ((length(data.dtot)>0) && (length(weight.dtot)>0)) {
			if (b.check[i]==0) {
				vals.data[i,data.ttod]=ts.data.use2[data.dtot]
			} else {
				vals.data[i,data.ttod]=ts.data.use2[data.dtot]/(0.5*max(ts.data.use2[data.dtot],na.rm=TRUE))
			}

			vals.weight[i,weight.ttod]=ts.weight.use2[weight.dtot]
		} else {
			dchecker[i]=0
		}
	}
	}
}



##########################
### Prefs data (and years) (not equal weighting) (0.5maxB as Bmsy proxy possible)
##########################

if (scen.prefs=="allTBandSSB" && scen.weights!="equal" && scen.type=="asmt-0.5maxB") {
	dchecker=numeric(NT)
	year.min=2020
	year.max=0
	vids=matrix("",ncol=3,nrow=NT)
	colnames(vids)=c("stockid","data","weight")
	b.check=numeric(NT)

	for (i in 1:NT) {
		stockid=fdat[i,"stockid"]
		ids.ptr=which(stockid==TSI[,"stockid"])
		vals.ptr=which(stockid==TSV[,"stockid"])
		years.ptr=which(stockid==timeseries_years_views[,"stockid"])
		vids[i,"stockid"]=stockid

		if (length(ids.ptr)>0) {
			dch=""
			ich=""
			if ((dch=="") && (!is.na(TSI[ids.ptr,"BdivBmsypref"]))) {dch="BdivBmsypref"}

			if ((dch=="") && (!is.na(TSI[ids.ptr,"TB"]))) {
				if (TSI[ids.ptr,"TB"]=="TB-MT") {
					dch="TB"
					b.check[i]=1
				}
			}
			if ((dch=="") && (!is.na(TSI[ids.ptr,"SSB"]))) {
				if (TSI[ids.ptr,"SSB"]=="SSB-MT") {
					dch="SSB"
					b.check[i]=1
				}
			}

			if ((ich=="") && (!is.na(TSI[ids.ptr,"TB"]))) {
				if (TSI[ids.ptr,"TB"]=="TB-MT") {ich="TB"}
			}
			if ((ich=="") && (!is.na(TSI[ids.ptr,"SSB"]))) {
				if (TSI[ids.ptr,"SSB"]=="SSB-MT") {ich="SSB"}
			}

			if (dch!="" && ich!="") {
				dchecker[i]=1
				yr.rng1=timeseries_years_views[years.ptr,dch]
				yr.str1=unlist(strsplit(yr.rng1,"-"))
				yr.min1=as.numeric(yr.str1[1])
				yr.max1=as.numeric(yr.str1[2])

				yr.rng2=timeseries_years_views[years.ptr,ich]
				yr.str2=unlist(strsplit(yr.rng2,"-"))
				yr.min2=as.numeric(yr.str2[1])
				yr.max2=as.numeric(yr.str2[2])

				yr.min=max(yr.min1,yr.min2)
				yr.max=min(yr.max1,yr.max2)
				if (year.min>yr.min) {year.min=yr.min}
				if (year.max<yr.max) {year.max=yr.max}
				vids[i,"data"]=dch
				vids[i,"weight"]=ich		
			}
		}
	}

	years=year.min:year.max
	if (scen.years=="paper") {years=1980:duarte.eyear}
	YT=length(years)

	vals.data=matrix(NA,ncol=length(years),nrow=NT)
	colnames(vals.data)=years
	vals.weight=matrix(NA,ncol=length(years),nrow=NT)
	colnames(vals.weight)=years

	for (i in 1:NT) {
	if (dchecker[i]==1) {
		stockid=fdat[i,"stockid"]
		tsv.ptr=which(stockid==TSV[,"stockid"])
		ts.data=as.numeric(TSV[tsv.ptr,vids[i,"data"]])
		ts.weight=TSV[tsv.ptr,vids[i,"weight"]]
		ts.year=TSV[tsv.ptr,"year"]

		ts.data.nna.ptr=which(!is.na(ts.data))
		ts.data.use=ts.data[min(ts.data.nna.ptr):max(ts.data.nna.ptr)]
		ts.data.year=ts.year[min(ts.data.nna.ptr):max(ts.data.nna.ptr)]

		ts.weight.nna.ptr=which(!is.na(ts.weight))
		ts.weight.use=ts.weight[min(ts.weight.nna.ptr):max(ts.weight.nna.ptr)]
		ts.weight.year=ts.year[min(ts.weight.nna.ptr):max(ts.weight.nna.ptr)]

		ts.use.year.min=max(min(ts.data.year),min(ts.weight.year))
		ts.use.year.max=min(max(ts.data.year),max(ts.weight.year))
		ts.use.year=ts.use.year.min:ts.use.year.max

		ts.data.use.ptr=which(ts.data.year %in% ts.use.year)
		ts.data.use2=ts.data.use[ts.data.use.ptr]
		ts.data.year2=ts.data.year[ts.data.use.ptr]

		ts.weight.use.ptr=which(ts.weight.year %in% ts.use.year)
		ts.weight.use2=ts.weight.use[ts.weight.use.ptr]
		ts.weight.year2=ts.weight.year[ts.weight.use.ptr]

		data.dtot=which(ts.data.year2 %in% years)
		data.ttod=which(years %in% ts.data.year2)

		weight.dtot=which(ts.weight.year2 %in% years)
		weight.ttod=which(years %in% ts.weight.year2)

		if ((length(data.dtot)>0) && (length(weight.dtot)>0)) {
			if (b.check[i]==0) {
				vals.data[i,data.ttod]=ts.data.use2[data.dtot]
			} else {
				vals.data[i,data.ttod]=ts.data.use2[data.dtot]/(0.5*max(ts.data.use2[data.dtot],na.rm=TRUE))
			}

			vals.weight[i,weight.ttod]=ts.weight.use2[weight.dtot]
		} else {
			dchecker[i]=0
		}
	}
	}
}



##########################
### Prefs data (and years) (equal weighting) (0.5maxB as Bmsy proxy possible)
##########################

if (scen.prefs=="allTBandSSB" && scen.weights=="equal" && scen.type=="asmt-0.5maxB") {
	dchecker=numeric(NT)
	year.min=2020
	year.max=0
	vids=matrix("",ncol=2,nrow=NT)
	colnames(vids)=c("stockid","data")
	b.check=numeric(NT)

	for (i in 1:NT) {
		stockid=fdat[i,"stockid"]
		ids.ptr=which(stockid==TSI[,"stockid"])
		vals.ptr=which(stockid==TSV[,"stockid"])
		years.ptr=which(stockid==timeseries_years_views[,"stockid"])
		vids[i,"stockid"]=stockid

		if (length(ids.ptr)>0) {
			dch=""
			if ((dch=="") && (!is.na(TSI[ids.ptr,"BdivBmsypref"]))) {dch="BdivBmsypref"}
			if ((dch=="") && (!is.na(TSI[ids.ptr,"TB"]))) {
				if (TSI[ids.ptr,"TB"]=="TB-MT") {
					dch="TB"
					b.check[i]=1
				}
			}
			if ((dch=="") && (!is.na(TSI[ids.ptr,"SSB"]))) {
				if (TSI[ids.ptr,"SSB"]=="SSB-MT") {
					dch="SSB"
					b.check[i]=1
				}
			}

			if (dch!="") {
				dchecker[i]=1
				yr.rng=timeseries_years_views[years.ptr,dch]
				yr.str=unlist(strsplit(yr.rng,"-"))
				yr.min=as.numeric(yr.str[1])
				yr.max=as.numeric(yr.str[2])
				if (year.min>yr.min) {year.min=yr.min}
				if (year.max<yr.max) {year.max=yr.max}
				vids[i,"data"]=dch
			}
		}
	}

	years=year.min:year.max
	if (scen.years=="paper") {years=1980:duarte.eyear}
	YT=length(years)

	vals.data=matrix(NA,ncol=length(years),nrow=NT)
	colnames(vals.data)=years

	for (i in 1:NT) {
	if (dchecker[i]==1) {
		stockid=fdat[i,"stockid"]
		tsv.ptr=which(stockid==TSV[,"stockid"])
		ts.data=as.numeric(TSV[tsv.ptr,vids[i,"data"]])
		ts.year=TSV[tsv.ptr,"year"]

		ts.data.nna.ptr=which(!is.na(ts.data))
		ts.data.use=ts.data[min(ts.data.nna.ptr):max(ts.data.nna.ptr)]
		ts.data.year=ts.year[min(ts.data.nna.ptr):max(ts.data.nna.ptr)]

		data.dtot=which(ts.data.year %in% years)
		data.ttod=which(years %in% ts.data.year)

		if (length(data.dtot)>0) {
			if (b.check[i]==0) {
				vals.data[i,data.ttod]=ts.data.use[data.dtot]
			} else {
				vals.data[i,data.ttod]=ts.data.use[data.dtot]/(0.5*max(ts.data.use[data.dtot],na.rm=TRUE))
			}
		} else {
			dchecker[i]=0
		}
	}
	}
}



########################################################################################################
########################################################################################################
########################################################################################################
########################################################################################################
########################################################################################################
########################################################################################################
########################################################################################################
########################################################################################################
########################################################################################################
########################################################################################################
###################################                        #############################################
###################################       Data Weighting   #############################################
###################################                        #############################################
########################################################################################################
########################################################################################################
########################################################################################################
########################################################################################################
########################################################################################################
########################################################################################################
########################################################################################################
########################################################################################################
########################################################################################################
########################################################################################################
########################################################################################################
###################
###################	Data is weighted by scenario weighting and scenario aggregate to generate
###################	final B/Bmsy time series.
###################



plot.summ.header=c("Scenario","Year","BdivBmsy","No. stocks in calc.",
	"RAM version","RAM type","Stock subset","Year range","Data source","BdivB prefs","Weighting","Aggregate")
plot.summ=matrix("",nrow=YT,ncol=length(plot.summ.header))
colnames(plot.summ)=plot.summ.header

plot.summ[,"Scenario"]=scen.scen
plot.summ[,"RAM version"]=scen.version
plot.summ[,"RAM type"]=scen.type
plot.summ[,"Stock subset"]=scen.stocks
plot.summ[,"Year range"]=scen.years
plot.summ[,"Data source"]="views"
plot.summ[,"BdivB prefs"]=scen.prefs
plot.summ[,"Weighting"]=scen.weights
plot.summ[,"Aggregate"]=scen.agg



##########################
### Equal weight (geom)
##########################

if (scen.weights=="equal" && scen.agg=="geom") {
	ts.plot.values=rep(NA,YT)

	for (i in 1:YT) {
		year.vals=vals.data[,i]
		nna.ptr=which(!is.na(year.vals))
		year.uvals=year.vals[nna.ptr]
		year.num=length(nna.ptr)

		ts.plot.values[i]=prod(year.uvals)^(1/year.num)

		plot.summ[i,"Year"]=years[i]
		plot.summ[i,"BdivBmsy"]=ts.plot.values[i]
		plot.summ[i,"No. stocks in calc."]=year.num
	}	
}



##########################
### Equal weight (median)
##########################

if (scen.weights=="equal" && scen.agg=="median") {
	ts.plot.values=rep(NA,YT)

	for (i in 1:YT) {
		year.vals=vals.data[,i]
		nna.ptr=which(!is.na(year.vals))
		year.uvals=year.vals[nna.ptr]
		year.num=length(nna.ptr)

		ts.plot.values[i]=median(year.uvals,na.rm=TRUE)

		plot.summ[i,"Year"]=years[i]
		plot.summ[i,"BdivBmsy"]=ts.plot.values[i]
		plot.summ[i,"No. stocks in calc."]=year.num
	}	
}



##########################
### yearBwt weight (geom)
##########################

if (scen.weights=="yearBwt" && scen.agg=="geom") {
	annual.totals=rep(NA,YT)

	for (i in 1:YT) {
		annual.totals[i]=sum(vals.weight[,i],na.rm=TRUE)
	}

	wt.prop=matrix(NA,ncol=length(years),nrow=NT)
	colnames(wt.prop)=years

	for (i in 1:NT) {
	if (dchecker[i]==1) {
	for (j in 1:YT) {
		wt.prop[i,j]=vals.weight[i,j]/annual.totals[j]
	}
	}
	}

	ts.plot.values=rep(NA,YT)

	for (i in 1:YT) {
		year.vals=vals.data[,i]
		year.weights=vals.weight[,i]
		nna1.ptr=which(!is.na(year.vals))
		nna2.ptr=which(!is.na(year.weights))
		nna.ptr=intersect(nna1.ptr,nna2.ptr)

		year.uvals=year.vals[nna.ptr]
		year.num=length(nna.ptr)
		year.uweights=wt.prop[nna.ptr,i]

		ts.plot.values[i]=prod(year.uvals^(year.uweights))^(1/sum(year.uweights))

		plot.summ[i,"Year"]=years[i]
		plot.summ[i,"BdivBmsy"]=ts.plot.values[i]
		plot.summ[i,"No. stocks in calc."]=year.num
	}
}



##########################
### Bmsywt weight (geom)
##########################

if (scen.weights=="Bmsywt" && scen.agg=="geom") {
	avgB=rep(NA,NT)

	for (i in 1:NT) {
	if (dchecker[i]==1) {
		tb.check=0
		ssb.check=0
		tbmsy.check=0
		ssbmsy.check=0
		ids.ptr=which(stockid==BPI[,"stockid"])
		vals.ptr=which(stockid==BPV[,"stockid"])
		if (length(ids.ptr)>0) {
			if (!is.na(BPI[ids.ptr,"TBmsybest"])) {tbmsy.check=1}
			if (!is.na(BPI[ids.ptr,"SSBmsy"])) {
				if (str_detect(BPI[ids.ptr,"SSBmsy"],"-MT")) {ssbmsy.check=1}
			}
		}
		if (vids[i,"weight"]=="TB") {tb.check=1}
		if (vids[i,"weight"]=="SSB") {ssb.check=1}
		store.check=0

		if (tbmsy.check==1 && store.check==0) {
			avgB[i]=BPV[vals.ptr,"TBmsybest"]
			store.check=1
		}
		if (tb.check==1 && store.check==0) {
			avgB[i]=mean(vals.weight[i,],na.rm=TRUE)
			store.check=1
		}
		if (ssbmsy.check==1 && store.check==0) {
			avgB[i]=BPV[vals.ptr,"SSBmsy"]
			store.check=1
		}
		if (ssb.check==1 && store.check==0) {
			avgB[i]=mean(vals.weight[i,],na.rm=TRUE)
			store.check=1
		}
	}
	}

	avgB.total=sum(avgB,na.rm=TRUE)
	avgB.prop=avgB/avgB.total

	ts.prop=matrix(NA,ncol=length(years),nrow=NT)
	colnames(ts.prop)=years

	ts.plot.values=rep(NA,YT)

	for (i in 1:YT) {

		year.vals=vals.data[,i]
		year.weights=vals.weight[,i]
		nna1.ptr=which(!is.na(year.vals))
		nna2.ptr=which(!is.na(year.weights))
		nna.ptr=intersect(nna1.ptr,nna2.ptr)

		year.uvals=year.vals[nna.ptr]
		year.num=length(nna.ptr)
		year.uweights=avgB.prop[nna.ptr]

		ts.plot.values[i]=prod(year.uvals^(year.uweights))^(1/sum(year.uweights))

		plot.summ[i,"Year"]=years[i]
		plot.summ[i,"BdivBmsy"]=ts.plot.values[i]
		plot.summ[i,"No. stocks in calc."]=year.num
	}
}



##########################
### Bmsywt weight (weighted median)
##########################

if (scen.weights=="Bmsywt" && scen.agg=="wtmedian") {
	avgB=rep(NA,NT)

	for (i in 1:NT) {
	if (dchecker[i]==1) {
		tb.check=0
		ssb.check=0
		tbmsy.check=0
		ssbmsy.check=0
		ids.ptr=which(stockid==BPI[,"stockid"])
		vals.ptr=which(stockid==BPV[,"stockid"])
		if (length(ids.ptr)>0) {
			if (!is.na(BPI[ids.ptr,"TBmsybest"])) {tbmsy.check=1}
			if (!is.na(BPI[ids.ptr,"SSBmsy"])) {
				if (str_detect(BPI[ids.ptr,"SSBmsy"],"-MT")) {ssbmsy.check=1}
			}
		}
		if (vids[i,"weight"]=="TB") {tb.check=1}
		if (vids[i,"weight"]=="SSB") {ssb.check=1}
		store.check=0

		if (tbmsy.check==1 && store.check==0) {
			avgB[i]=BPV[vals.ptr,"TBmsybest"]
			store.check=1
		}
		if (tb.check==1 && store.check==0) {
			avgB[i]=mean(vals.weight[i,],na.rm=TRUE)
			store.check=1
		}
		if (ssbmsy.check==1 && store.check==0) {
			avgB[i]=BPV[vals.ptr,"SSBmsy"]
			store.check=1
		}
		if (ssb.check==1 && store.check==0) {
			avgB[i]=mean(vals.weight[i,],na.rm=TRUE)
			store.check=1
		}

	}
	}

	avgB.total=sum(avgB,na.rm=TRUE)
	avgB.prop=avgB/avgB.total

	ts.prop=matrix(NA,ncol=length(years),nrow=NT)
	colnames(ts.prop)=years

	ts.plot.values=rep(NA,YT)

	for (i in 1:YT) {
		year.vals=vals.data[,i]
		year.weights=vals.weight[,i]
		nna1.ptr=which(!is.na(year.vals))
		nna2.ptr=which(!is.na(year.weights))
		nna.ptr=intersect(nna1.ptr,nna2.ptr)

		year.uvals=year.vals[nna.ptr]
		year.num=length(nna.ptr)
		year.uweights=avgB.prop[nna.ptr]

		ts.plot.values[i]=weighted.median(year.uvals,year.uweights,na.rm=TRUE)

		plot.summ[i,"Year"]=years[i]
		plot.summ[i,"BdivBmsy"]=ts.plot.values[i]
		plot.summ[i,"No. stocks in calc."]=year.num
	}
}



########################################################################################################
########################################################################################################
########################################################################################################
########################################################################################################
########################################################################################################
########################################################################################################
########################################################################################################
########################################################################################################
########################################################################################################
########################################################################################################
###################################                        #############################################
###################################       Data Output      #############################################
###################################                        #############################################
########################################################################################################
########################################################################################################
########################################################################################################
########################################################################################################
########################################################################################################
########################################################################################################
########################################################################################################
########################################################################################################
########################################################################################################
########################################################################################################
########################################################################################################
###################
###################	Data is output to a csv file.
###################

##########################
### Output Summary File
##########################
setwd(main_dir)
setwd("./scenarios/")
write.table(plot.summ,file=paste("Scenario ",scen.scen," Summary.csv",sep=""), row.names=FALSE, col.names=TRUE, sep=",")

setwd(main_dir)

}





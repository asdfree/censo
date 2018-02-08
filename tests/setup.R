if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )

options("lodown.cachaca.savecache"=FALSE)

this_sample_break <- Sys.getenv( "this_sample_break" )

library(lodown)

censo_cat <-
	get_catalog( "censo" ,
		output_dir = file.path( getwd() ) )

censo_cat <- censo_cat[ split( seq( nrow( censo_cat ) ) , 1 + sort( seq( nrow( censo_cat ) ) %% 55 ) )[[ this_sample_break ]] , ]

censo_cat <- lodown( "censo" , censo_cat )

if( all( censo_cat$year == 2010 ) ){











library(survey)

# choose columns to import from both household and person files
columns_to_import <-
	c( 'v6531' , 'v6033' , 'v0640' , 'v0001' , 'v0601' )

# initiate a data.frame to stack all downloaded censo states
censo_df <- data.frame( NULL )
		
# only construct one censo design at a time (2000 and 2010 should not be stacked)
stopifnot( length( unique( censo_cat[ , 'year' ] ) ) == 1 )
		
# loop through all downloaded censo states
for( this_state in seq( nrow( censo_cat ) ) ){
	
	# add the design information to the columns to import
	these_columns_to_import <-
		unique( 
			c( 
				columns_to_import , 
				as.character( 
					censo_cat[ this_state , c( 'weight' , paste0( 'fpc' , 1:5 ) ) ] 
				) 
			) 
		)

	# remove NAs
	these_columns_to_import <- these_columns_to_import[ !is.na( these_columns_to_import ) ]

	# load structure files, lowercase variable names, set unwanted columns to missing
	dom_stru <- SAScii::parse.SAScii( censo_cat[ this_state , 'dom_sas' ] )
	dom_stru$varname <- tolower( dom_stru$varname )
	
	pes_stru <- SAScii::parse.SAScii( censo_cat[ this_state , 'pes_sas' ] )
	pes_stru$varname <- tolower( pes_stru$varname )
	
	# import fixed-width files
	this_censo_dom_df <- 
		data.frame( readr::read_fwf(
			censo_cat[ this_state , 'dom_file' ] ,
			readr::fwf_widths( 
				abs( dom_stru$width ) , col_names = dom_stru[ , 'varname' ] 
			) ,
			col_types = 
				paste0( 
					ifelse( !( dom_stru$varname %in% these_columns_to_import ) , 
						"_" , 
						ifelse( dom_stru$char , "c" , "d" ) 
					) , 
					collapse = "" 
				)
		) )

	this_censo_pes_df <- 
		data.frame( readr::read_fwf(
			censo_cat[ this_state , 'pes_file' ] ,
			readr::fwf_widths( 
				abs( pes_stru$width ) , col_names = pes_stru[ , 'varname' ] 
			) ,
			col_types = 
				paste0( 
					ifelse( !( pes_stru$varname %in% these_columns_to_import ) , 
						"_" , 
						ifelse( pes_stru$char , "c" , "d" ) 
					) , 
					collapse = "" 
				)
		) )

	# add decimals
	for( this_variable in these_columns_to_import ) {
	
		if( 
			( this_variable %in% names( this_censo_dom_df ) ) & 
			!isTRUE( all.equal( 1 , dom_stru[ dom_stru$varname == this_variable , 'divisor' ] ) ) 
		){
			this_censo_dom_df[ , this_variable ] <- 
				dom_stru[ dom_stru$varname == this_variable , 'divisor' ] * 
				this_censo_dom_df[ , this_variable ]
		}
	
		if( 
			( this_variable %in% names( this_censo_pes_df ) ) & 
			!isTRUE( all.equal( 1 , pes_stru[ pes_stru$varname == this_variable , 'divisor' ] ) ) 
		){
			this_censo_pes_df[ , this_variable ] <- 
				pes_stru[ pes_stru$varname == this_variable , 'divisor' ] * 
				this_censo_pes_df[ , this_variable ]
		}
	
	}

	# merge household and person tables
	this_censo_df <- merge( this_censo_dom_df , this_censo_pes_df )

	# confirm one record per person, with household information merged on
	stopifnot( nrow( this_censo_df ) == nrow( this_censo_pes_df ) )
	
	rm( this_censo_dom_df , this_censo_pes_df ) ; gc()
	
	# stack the merged tables
	censo_df <- rbind( censo_df , this_censo_df )
	
	rm( this_censo_df ) ; gc()
	
}

# add a column of ones
censo_df[ , 'one' ] <- 1

# calculate the finite population correction for each stratum to construct a
# sampling design with weighting areas as strata and households as psu

# v0010 is the person or household weight
# v0011 is the weighting area identifier
# both of these are specified inside `censo_cat[ c( 'fpc1' , 'weight' ) ]`

fpc_sums <- aggregate( v0010 ~ v0011 , data = censo_df , sum )

names( fpc_sums )[ 2 ] <- 'fpc'

censo_df <- merge( censo_df , fpc_sums ) ; gc()

censo_wgts <-
	survey::bootweights(
		strata = censo_df[ , censo_cat[ 1 , 'fpc1' ] ] ,
		psu = censo_df[ , censo_cat[ 1 , 'fpc4' ] ] ,
		replicates = 80 ,
		fpc = censo_df[ , 'fpc' ]
	)

# construct a complex survey design object
censo_design <-
	survey::svrepdesign(
		weight = ~ v0010 ,
		repweights = censo_wgts$repweights ,
		type = "bootstrap",
		combined.weights = FALSE ,
		scale = censo_wgts$scale ,
		rscales = censo_wgts$rscales ,
		data = censo_df
	)
	
rm( censo_df , censo_wgts , fpc_sums ) ; gc()
censo_design <-
	update(
		
		censo_design ,
		
		nmorpob1 = ifelse( v6531 >= 0 , as.numeric( v6531 < 70 ) , NA ) ,
		nmorpob2 = ifelse( v6531 >= 0 , as.numeric( v6531 < 80 ) , NA ) , 
		nmorpob3 = ifelse( v6531 >= 0 , as.numeric( v6531 < 90 ) , NA ) , 
		nmorpob4 = ifelse( v6531 >= 0 , as.numeric( v6531 < 100 ) , NA ) , 
		nmorpob5 = ifelse( v6531 >= 0 , as.numeric( v6531 < 140 ) , NA ) , 
		nmorpob6 = ifelse( v6531 >= 0 , as.numeric( v6531 < 272.50 ) , NA ) ,
		
		sexo = factor( v0601 , labels = c( "masculino" , "feminino" ) ) ,
		
		state_name = 
			factor( 
				v0001 , 
				levels = c( 11:17 , 21:29 , 31:33 , 35 , 41:43 , 50:53 ) ,
				labels = c( "Rondonia" , "Acre" , "Amazonas" , 
				"Roraima" , "Para" , "Amapa" , "Tocantins" , 
				"Maranhao" , "Piaui" , "Ceara" , "Rio Grande do Norte" , 
				"Paraiba" , "Pernambuco" , "Alagoas" , "Sergipe" , 
				"Bahia" , "Minas Gerais" , "Espirito Santo" , 
				"Rio de Janeiro" , "Sao Paulo" , "Parana" , 
				"Santa Catarina" , "Rio Grande do Sul" , 
				"Mato Grosso do Sul" , "Mato Grosso" , "Goias" , 
				"Distrito Federal" )
			)
	)
sum( weights( censo_design , "sampling" ) != 0 )

svyby( ~ one , ~ state_name , censo_design , unwtd.count )
svytotal( ~ one , censo_design )

svyby( ~ one , ~ state_name , censo_design , svytotal )
svymean( ~ v6033 , censo_design )

svyby( ~ v6033 , ~ state_name , censo_design , svymean )
svymean( ~ sexo , censo_design )

svyby( ~ sexo , ~ state_name , censo_design , svymean )
svytotal( ~ v6033 , censo_design )

svyby( ~ v6033 , ~ state_name , censo_design , svytotal )
svytotal( ~ sexo , censo_design )

svyby( ~ sexo , ~ state_name , censo_design , svytotal )
svyquantile( ~ v6033 , censo_design , 0.5 )

svyby( 
	~ v6033 , 
	~ state_name , 
	censo_design , 
	svyquantile , 
	0.5 ,
	ci = TRUE ,
	keep.var = TRUE 
)
svyratio( 
	numerator = ~ nmorpob1 , 
	denominator = ~ nmorpob1 + one , 
	censo_design ,
	na.rm = TRUE
)
sub_censo_design <- subset( censo_design , v0640 == 1 )
svymean( ~ v6033 , sub_censo_design )
this_result <- svymean( ~ v6033 , censo_design )

coef( this_result )
SE( this_result )
confint( this_result )
cv( this_result )

grouped_result <-
	svyby( 
		~ v6033 , 
		~ state_name , 
		censo_design , 
		svymean 
	)
	
coef( grouped_result )
SE( grouped_result )
confint( grouped_result )
cv( grouped_result )
degf( censo_design )
svyvar( ~ v6033 , censo_design )
# SRS without replacement
svymean( ~ v6033 , censo_design , deff = TRUE )

# SRS with replacement
svymean( ~ v6033 , censo_design , deff = "replace" )
svyciprop( ~ nmorpob6 , censo_design ,
	method = "likelihood" , na.rm = TRUE )
svyttest( v6033 ~ nmorpob6 , censo_design )
svychisq( 
	~ nmorpob6 + sexo , 
	censo_design 
)
glm_result <- 
	svyglm( 
		v6033 ~ nmorpob6 + sexo , 
		censo_design 
	)

summary( glm_result )
library(convey)
censo_design <- convey_prep( censo_design )

sub_censo_design <- 
	subset( censo_design , v6531 >= 0 )

svygini( ~ v6531 , sub_censo_design , na.rm = TRUE )

}

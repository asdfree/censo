if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )

options("lodown.cachaca.savecache"=FALSE)

this_sample_break <- Sys.getenv( "this_sample_break" )

library(lodown)

censo_cat <-
	get_catalog( "censo" ,
		output_dir = file.path( getwd() ) )

censo_cat <- censo_cat[ split( seq( nrow( censo_cat ) ) , 1 + sort( seq( nrow( censo_cat ) ) %% 11 ) )[[ this_sample_break ]] , ]

censo_cat <- lodown( "censo" , censo_cat )

library(lodown)
# examine all available CENSO microdata files
censo_cat <-
	get_catalog( "censo" ,
		output_dir = file.path( getwd() ) )

# 2010 only
censo_cat <- subset( censo_cat , year == 2010 )
# download the microdata to your local computer


library(survey)

options( survey.lonely.psu = "adjust" )

censo_design <- readRDS( file.path( getwd() , "pes 2010 design.rds" ) )

censo_design <- open( censo_design , driver = SQLite() )
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


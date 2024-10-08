/*
################################################################    Kostelec Czech Silviculture Management     ###########################################################################
##########################################################                      Shalterwood Cut                         ########################################################################
                                                                 NEW MANAGEMENT ADAPTATION TO THE GLOBAL CHANGES
                                                                                      
This management script is doing adaptation management activities on the full landscape of our study area in the Kostelec for iLand model.
MARCO BALDO
2024/08/03
We have one agent and one unit. 1sw to 5sw stps
This selection was done in preprocess (inside R environment).
Activities that this script is doing:
Planting x9
Thinning x9 x5 stand treatment programs
Shalterwood cut x1
Salvaging type x1
value in csv for abe 'group'	site type	
1	1	Exposed and extreme sites (including acidic sites at lower elevations)
2	3	Acidic sites
3	5	Rich sites
4	7a	Waterlogged sites (oak type)
5	7b	Waterlogged sites (alnus type)
6	3	Acidic sites
 */



//--- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
//                                                 ---       P L A N T I N G  ----
//
//  At the input file there are columns with the fractions how planting should be done: PlantPiab	PlantAbal	PlantPisy	PlantFasy	PlantLade	PlantBrLf
//  Values are summing up to 1.  The last one is BrLf means broadleaf, and distributed into acps (40%), frex(30%) and ulgl(30%). 
//  Defines a fraction 0-1 of 2x2m pixels for which to plant trees of the given species. The `fraction` is interpreted as a probability, i.e. for each 2x2m pixel a random number decides whether to plant or not.
//  Planting is in 2nd year of every rotation (schedule: 2 )
//  50 cm high trees are planted wall to wall
//  We are not clearing the existing tree saplings at the site. (clear: false)

// STP1 planting_1 = planting for site 1 extreme and expose soils 80% Quercus petreae ( Sessile oak) and 20% Pinus sylvestris (Scotch pine).

var planting_1_adp = {
	 type: "planting", 
	 schedule: 2 ,    
	 items: [ {species: "qupe",  height: 0.5, fraction: 0.4 , clear: false},
		  {species: "qupu",  height: 0.5, fraction: 0.3 	},
		  {species: "pisy",  height: 0.5, fraction: 0.2 	},
		  {species: "lade",  height: 0.5, fraction: 0.1 	}],
		  
// Here just doing logging to logfile:
	 onExit: function() { 	fmengine.log("Planting qupe fraction= 0.4" );
    			     	    fmengine.log("Planting qupu fraction= 0.3" );
                            fmengine.log("Planting pisy fraction= 0.2" );
                            fmengine.log("Planting lade fraction= 0.1" );}
};

// STP2  planting_3 = planting for site 3 acidic soils 40% Quercus petreae L. ( Sessile oak) and 60% Pinus sylvestris L. (Scotch pine).


var planting_3_adp = {
	 type: "planting", 
	 schedule: 2 ,    
	 items: [ {species: "qupe",  height: 0.5, fraction: 0.4 , clear: false},
		      {species: "lade",  height: 0.5, fraction: 0.3 	},
			  {species: "pisy",  height: 0.5, fraction: 0.2 	},
			  {species: "abal",  height: 0.5, fraction: 0.1 	}],
		  
// Here just doing logging to logfile:
	 onExit: function() { 	fmengine.log("Planting qupe fraction= 0.4" );
    			     	    fmengine.log("Planting lade fraction= 0.2" );
                            fmengine.log("Planting pisy fraction= 0.2" );
                            fmengine.log("Planting abal fraction= 0.2" );}
};

// STP3 planting_5 = planting for site 5 rich soils 60% Picea abies L. (Norway sprunce) and 40% Fagus sylvatica L. (European beech).


var planting_5_adp = {
	 type: "planting", 
	 schedule: 2 ,    
	 items: [ {species: "qupe",  height: 0.5, fraction: 0.3 , clear: false},
		      {species: "abal",  height: 0.5, fraction: 0.3 	},
			  {species: "fasy",  height: 0.5, fraction: 0.2 	},
			  {species: "lade",  height: 0.5, fraction: 0.2 	}],
		  
// Here just doing logging to logfile:
	 onExit: function() { 	fmengine.log("Planting qupe fraction= 0.3" );
    			     	    fmengine.log("Planting abal fraction= 0.3" );
                            fmengine.log("Planting fasy fraction= 0.2" );
							fmengine.log("Planting lade fraction= 0.2" );}
};

// STP4 planting_7a = planting for site 7a partially waterlock soil 60% Picea abies L. (Norway sprunce) and 40% Quercus petra L. (Sessile oak).


var planting_7a_adp = {
	 type: "planting", 
	 schedule: 2 ,    
	 items: [ {species: "qupe",  height: 0.5, fraction: 0.3 , clear: false},
			  {species: "abal",  height: 0.5, fraction: 0.3 	},
			  {species: "pisy",  height: 0.5, fraction: 0.2 	},
			  {species: "lade",  height: 0.5, fraction: 0.2 	}],
		  
// Here just doing logging to logfile:
	 onExit: function() { 	fmengine.log("Planting qupe fraction= 0.3" );
                            fmengine.log("Planting abal fraction= 0.3" );
                            fmengine.log("Planting pisy fraction= 0.2" );
							fmengine.log("Planting lade fraction= 0.2" );}
};


// STP5 planting_7b = planting for site 7b waterlock soil 100% Alnus glutinosa L. (European alder).


var planting_7b_adp = {
	 type: "planting", 
	 schedule: 2 ,    
	 items: [ {species: "quro",  height: 0.5, fraction: 0.4 , clear: false},
			  {species: "abal",  height: 0.5, fraction: 0.2 	},
			  {species: "pisy",  height: 0.5, fraction: 0.2 	},
			  {species: "algl",  height: 0.5, fraction: 0.2 	}],
		  
// Here just doing logging to logfile:
	 onExit: function() { 	fmengine.log("Planting quro fraction= 0.4" );
    			     	    fmengine.log("Planting abal fraction= 0.2" );
                            fmengine.log("Planting pisy fraction= 0.2" );
                            fmengine.log("Planting algl fraction= 0.2" );}
};


//--- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
//                                                      ---  S A L V A G I N G  --->


var salvager_sw = { type: 'salvage',  schedule: { repeat: true },
     disturbanceCondition: "dbh>10 and rnd(0,1)<"+Globals.setting('user.salvage.remove'),    // this part connect the js at the xml + this +Globals.setting('user.salvage.remove' is for calling where in the modules of the xml)
     onExecute: function() {
		 // when stand is cleared... if the damage > thresholdClearStand (e.g more than 80% of the area)
		 stand.trees.loadAll();

		 stand.trees.harvest("dbh>5"); 
		  
                   //fmengine.log("Remained after disturbance: " + trees.count + " normal trees");
                  //var n = trees.killSaplings('');
                  //fmengine.log("Plus remained: " + n + " saplings");
                 //trees.killSaplings("");
                //fmengine.log("We do not do harvest, and clearing, one RESET")

               // fmengine.runActivity(stand.id, "planting"); // assuming the name of the activity is "planting"      
               //  fmengine.log("PLANTING AFTER RESET");
		       //stand.trace = true; // enable tracing ...
			//stand.U = 120;   // stand.U is for the rotation time restarting after the salvaging
	 },

	 debugSplit: false,
	 thresholdIgnoreDamage: 500, // m3/ha  modified, WR: was 100000  above this threshold clearing and splitting is tested
	 thresholdClearStand: 0.8,  // This is the % of the demaged area in a stand. We can change it based on what we want. added WR  if the relative damage is higher than this, it is cleared
	 thresholdSplitStand: 1.0,   // added WR (no splitting=1, if 0.5 at 50% demaged area we split the stand based on demaged - no demaged area)   if this is smaller than the clearstand, it is splitted if rel.damage higher than this value

    }	



//--- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
//                                                      ---  T H I N N I N G S  --->   

// http://iland-model.org/ABE+activity+thinning
// For thinnings there are build in method in the Bilek L., excel file, and we use it. Stand top height and dbh variable CHANGE DURING THE ACTIVITY/AGE
// WE ARE USING A DIFFERENT METHOD FOR ANY KIND OF SOIL (extreme and expose soils site 1, acidic soils site 3, rich soils site 5, partially waterlock soils Oak type site 7a, waterlock soils Alder type site 7b. 
// (only for shelterwood) WE ARE USING SHELTERWOOD MANAGEMENT FOR LAST THINNING
// VARIANT FOR NUMBER OF STEMS REMAINING AND WITHOUT HEIGHT CONSTRAINS + volume target
// For stems constrains I made 5 trees in more of the target because was > of some n. i.g. thin 11 -> stems n. > 3050 = remainingStems: 3055

///////////////////////////////////////           THINNING STP1 sw       //////////////////////////////////////////////////////

var thinning11 = { type: "thinning", 
				   schedule: { minRel: 0.09, optRel: 0.10, maxRel: 0.11 , force=true},
				   
                                   thinning: "custom",
				   onEvaluate: function() { console.log("1st THINNING ");return true;},
				    remainingStems: 3055, targetValue: 25,  targetVariable: "volume", targetRelative: true,  minDbh: 0,
				   classes: [30, 30, 20, 10, 10]
}



var thinning12 = { type: "thinning", 
				   schedule: { minRel: 0.19, optRel: 0.20, maxRel: 0.21 , force=true},
				   
                                   thinning: "custom",
				   onEvaluate: function() { console.log("2nd THINNING ");return true;},
				   remainingStems: 855, targetValue: 25,  targetVariable: "volume", targetRelative: true,  minDbh: 5,
				   classes: [30, 30, 20, 10, 10]
}



var thinning13 = { type: "thinning", 
				   schedule: { minRel: 0.29, optRel: 0.30, maxRel: 0.31 , force=true},
				   
                                   thinning: "custom",
				   onEvaluate: function() { console.log("3rd THINNING ");return true;},
				   remainingStems: 505, targetValue: 25,  targetVariable: "volume", targetRelative: true,  minDbh: 5,
				   classes: [30, 30, 30, 10, 0]
}


var thinning14 = { type: "thinning", 
				   schedule: { minRel: 0.39, optRel: 0.40, maxRel: 0.41 , force=true},
				   
                                   thinning: "custom",
				   onEvaluate: function() { console.log("4th THINNING ");return true;},
				   remainingStems: 305, targetValue: 20,  targetVariable: "volume", targetRelative: true,  minDbh: 5,
				   classes: [30, 30, 30, 10, 0]
}


var thinning15 = { type: "thinning", 
				   schedule: { minRel: 0.49, optRel: 0.50, maxRel: 0.51 , force=true},
				   
                                   thinning: "custom",
				   onEvaluate: function() { console.log("5th THINNING ");return true;},
				   remainingStems: 205, targetValue: 15,  targetVariable: "volume", targetRelative: true,  minDbh: 5,
				   classes: [30, 30, 30, 10, 0]
}



var thinning16 = { type: "thinning", 
				   schedule: { minRel: 0.59, optRel: 0.60, maxRel: 0.61 , force=true},
				   
                                   thinning: "custom",
				   onEvaluate: function() { console.log("6th THINNING ");return true;},
				   remainingStems: 155, targetValue: 15,  targetVariable: "volume", targetRelative: true,  minDbh: 5,
				   classes: [30, 30, 30, 10, 0]
}


var thinning17 = { type: "thinning", 
				   schedule: { minRel: 0.69, optRel: 0.70, maxRel: 0.71 , force=true},
				   
                                   thinning: "custom",
				   onEvaluate: function() { console.log("7th THINNING ");return true;},
				   remainingStems: 125, targetValue: 15,  targetVariable: "volume", targetRelative: true,  minDbh: 5,
				   classes: [30, 30, 30, 10, 0]
}

// IF THE FLUCTUATION COMPERING THE CC MANAGEMENT ARE EVEN HIGHER THE PROBLEM COULD BE THE FIRST SW THINNING

var thinning1_sw1 = { type: "thinning", 
				   schedule: { minRel: 0.79, optRel: 0.80, maxRel: 0.81 , force=true},
				   
                                 thinning: "custom",
				   onEvaluate: function() { console.log("8th THINNING sw1 ");return true;},
				   targetValue: 40,  targetVariable: "volume", targetRelative: true,  minDbh: 5,
				   classes: [40, 30, 20, 5, 5]
}


var thinning1_sw2 = { type: "thinning", 
				   schedule: { minRel: 0.89, optRel: 0.90, maxRel: 0.91 , force=true},
				   
                                 thinning: "custom",
				   onEvaluate: function() { console.log("9th THINNING sw2 ");return true;},
				   targetValue: 60,  targetVariable: "volume", targetRelative: true,  minDbh: 5,
				   classes: [20, 20, 20, 20, 20]
}

///////////////////////////////////////           THINNING STP2 sw       //////////////////////////////////////////////////////

var thinning31 = { type: "thinning", 
				   schedule: { minRel: 0.09, optRel: 0.10, maxRel: 0.11 , force=true},
				   
                                   thinning: "custom",
				   onEvaluate: function() { console.log("1st THINNING ");return true;},
				    remainingStems: 2005, targetValue: 30,  targetVariable: "volume", targetRelative: true,  minDbh: 0,
				   classes: [30, 30, 20, 10, 10]
}



var thinning32 = { type: "thinning", 
				   schedule: { minRel: 0.19, optRel: 0.20, maxRel: 0.21 , force=true},
				   
                                   thinning: "custom",
				   onEvaluate: function() { console.log("2nd THINNING ");return true;},
				   remainingStems: 1055, targetValue: 30,  targetVariable: "volume", targetRelative: true,  minDbh: 5,
				   classes: [30, 30, 20, 10, 10]
}



var thinning33 = { type: "thinning", 
				   schedule: { minRel: 0.29, optRel: 0.30, maxRel: 0.31 , force=true},
				   
                                   thinning: "custom",
				   onEvaluate: function() { console.log("3rd THINNING ");return true;},
				   remainingStems: 705, targetValue: 30,  targetVariable: "volume", targetRelative: true,  minDbh: 5,
				   classes: [30, 30, 30, 10, 0]
}


var thinning34 = { type: "thinning", 
				   schedule: { minRel: 0.39, optRel: 0.40, maxRel: 0.41 , force=true},
				   
                                   thinning: "custom",
				   onEvaluate: function() { console.log("4th THINNING ");return true;},
				   remainingStems: 455, targetValue: 20,  targetVariable: "volume", targetRelative: true,  minDbh: 5,
				   classes: [30, 30, 30, 10, 0]
}


var thinning35 = { type: "thinning", 
				   schedule: { minRel: 0.49, optRel: 0.50, maxRel: 0.51 , force=true},
				   
                                   thinning: "custom",
				   onEvaluate: function() { console.log("5th THINNING ");return true;},
				   remainingStems: 375, targetValue: 15,  targetVariable: "volume", targetRelative: true,  minDbh: 5,
				   classes: [30, 30, 30, 10, 0]
}



var thinning36 = { type: "thinning", 
				   schedule: { minRel: 0.59, optRel: 0.60, maxRel: 0.61 , force=true},
				   
                                   thinning: "custom",
				   onEvaluate: function() { console.log("6th THINNING ");return true;},
				   remainingStems: 265, targetValue: 15,  targetVariable: "volume", targetRelative: true,  minDbh: 5,
				   classes: [30, 30, 30, 10, 0]
}


var thinning37 = { type: "thinning", 
				   schedule: { minRel: 0.69, optRel: 0.70, maxRel: 0.71 , force=true},
				   
                                   thinning: "custom",
				   onEvaluate: function() { console.log("7th THINNING ");return true;},
				   remainingStems: 255, targetValue: 15,  targetVariable: "volume", targetRelative: true,  minDbh: 5,
				   classes: [30, 30, 30, 10, 0]
}


var thinning3_sw1 = { type: "thinning", 
				   schedule: { minRel: 0.79, optRel: 0.80, maxRel: 0.81 , force=true},
				   
                                 thinning: "custom",
				   onEvaluate: function() { console.log("8th THINNING sw");return true;},
				   targetValue: 40,  targetVariable: "volume", targetRelative: true,  minDbh: 5,
				   classes: [40, 30, 20, 5, 5]
}


var thinning3_sw2 = { type: "thinning", 
				   schedule: { minRel: 0.89, optRel: 0.90, maxRel: 0.91 , force=true},
				   
                                 thinning: "custom",
				   onEvaluate: function() { console.log("9th THINNING sw");return true;},
				   targetValue: 60,  targetVariable: "volume", targetRelative: true,  minDbh: 5,
				   classes: [20, 20, 20, 20, 20]
}

///////////////////////////////////////           THINNING STP3 sw        //////////////////////////////////////////////////////


var thinning51 = { type: "thinning", 
				   schedule: { minRel: 0.09, optRel: 0.10, maxRel: 0.11 , force=true},
				   
                                   thinning: "custom",
				   onEvaluate: function() { console.log("1st THINNING ");return true;},
				    remainingStems: 1505, targetValue: 40,  targetVariable: "volume", targetRelative: true,  minDbh: 0,
				   classes: [10, 20, 25, 25, 20]
}



var thinning52 = { type: "thinning", 
				   schedule: { minRel: 0.19, optRel: 0.20, maxRel: 0.21 , force=true},
				   
                                   thinning: "custom",
				   onEvaluate: function() { console.log("2nd THINNING ");return true;},
				   remainingStems: 1005, targetValue: 30,  targetVariable: "volume", targetRelative: true,  minDbh: 5,
				   classes: [10, 20, 25, 25, 20]
}



var thinning53 = { type: "thinning", 
				   schedule: { minRel: 0.29, optRel: 0.30, maxRel: 0.31 , force=true},
				   
                                   thinning: "custom",
				   onEvaluate: function() { console.log("3rd THINNING ");return true;},
				   remainingStems: 705, targetValue: 20,  targetVariable: "volume", targetRelative: true,  minDbh: 5,
				   classes: [10, 20, 25, 25, 20]
}


var thinning54 = { type: "thinning", 
				   schedule: { minRel: 0.39, optRel: 0.40, maxRel: 0.41 , force=true},
				   
                                   thinning: "custom",
				   onEvaluate: function() { console.log("4th THINNING ");return true;},
				   remainingStems: 455, targetValue: 15,  targetVariable: "volume", targetRelative: true,  minDbh: 5,
				   classes: [10, 20, 25, 25, 20]
}


var thinning55 = { type: "thinning", 
				   schedule: { minRel: 0.49, optRel: 0.50, maxRel: 0.51 , force=true},
				   
                                   thinning: "custom",
				   onEvaluate: function() { console.log("5th THINNING ");return true;},
				   remainingStems: 385, targetValue: 15,  targetVariable: "volume", targetRelative: true,  minDbh: 5,
				   classes: [10, 20, 25, 25, 20]
}



var thinning56 = { type: "thinning", 
				   schedule: { minRel: 0.59, optRel: 0.60, maxRel: 0.61 , force=true},
				   
                                   thinning: "custom",
				   onEvaluate: function() { console.log("6th THINNING ");return true;},
				   remainingStems: 335, targetValue: 15,  targetVariable: "volume", targetRelative: true,  minDbh: 5,
				   classes: [30, 30, 30, 10, 0]
}


var thinning5_sw1 = { type: "thinning", 
				   schedule: { minRel: 0.69, optRel: 0.70, maxRel: 0.71 , force=true},
				   
                                   thinning: "custom",
				   onEvaluate: function() { console.log("7th THINNING sw");return true;},
				   targetValue: 25,  targetVariable: "volume", targetRelative: true,  minDbh: 5,
				   classes: [40, 30, 20, 5, 5]
}


var thinning5_sw2 = { type: "thinning", 
				   schedule: { minRel: 0.79, optRel: 0.80, maxRel: 0.81 , force=true},
				   
                                   thinning: "custom",
				   onEvaluate: function() { console.log("8th THINNING sw");return true;},
				   targetValue: 40,  targetVariable: "volume", targetRelative: true,  minDbh: 5,
				   classes: [20, 20, 20, 20, 20]
}


var thinning5_sw3 = { type: "thinning", 
				   schedule: { minRel: 0.89, optRel: 0.90, maxRel: 0.91 , force=true},
				   
                                 thinning: "custom",
				   onEvaluate: function() { console.log("9th THINNING sw");return true;},
				   targetValue: 55,  targetVariable: "volume", targetRelative: true,  minDbh: 5,
				   classes: [20, 20, 20, 20, 20]
}

///////////////////////////////////////           THINNING STP4 sw        //////////////////////////////////////////////////////


var thinning71a = { type: "thinning", 
				   schedule: { minRel: 0.09, optRel: 0.10, maxRel: 0.11 , force=true},
				   
                                   thinning: "custom",
				   onEvaluate: function() { console.log("1st THINNING ");return true;},
				    remainingStems: 1505, targetValue: 45,  targetVariable: "volume", targetRelative: true,  minDbh: 0,
				   classes: [10, 20, 25, 25, 20]
}


var thinning72a = { type: "thinning", 
				   schedule: { minRel: 0.19, optRel: 0.20, maxRel: 0.21 , force=true},
				   
                                   thinning: "custom",
				   onEvaluate: function() { console.log("2nd THINNING ");return true;},
				   remainingStems: 1005, targetValue: 30,  targetVariable: "volume", targetRelative: true,  minDbh: 5,
				   classes: [10, 20, 25, 25, 20]
}


var thinning73a = { type: "thinning", 
				   schedule: { minRel: 0.29, optRel: 0.30, maxRel: 0.31 , force=true},
				   
                                   thinning: "custom",
				   onEvaluate: function() { console.log("3rd THINNING ");return true;},
				   remainingStems: 705, targetValue: 20,  targetVariable: "volume", targetRelative: true,  minDbh: 5,
				   classes: [10, 20, 25, 25, 20]
}


var thinning74a = { type: "thinning", 
				   schedule: { minRel: 0.39, optRel: 0.40, maxRel: 0.41 , force=true},
				   
                                   thinning: "custom",
				   onEvaluate: function() { console.log("4th THINNING ");return true;},
				   remainingStems: 455, targetValue: 15,  targetVariable: "volume", targetRelative: true,  minDbh: 5,
				   classes: [10, 20, 25, 25, 20]
}


var thinning75a = { type: "thinning", 
				   schedule: { minRel: 0.49, optRel: 0.50, maxRel: 0.51 , force=true},
				   
                                   thinning: "custom",
				   onEvaluate: function() { console.log("5th THINNING ");return true;},
				   remainingStems: 385, targetValue: 15,  targetVariable: "volume", targetRelative: true,  minDbh: 5,
				   classes: [10, 20, 25, 25, 20]
}


var thinning76a = { type: "thinning", 
				   schedule: { minRel: 0.59, optRel: 0.60, maxRel: 0.61 , force=true},
				   
                                   thinning: "custom",
				   onEvaluate: function() { console.log("6th THINNING ");return true;},
				   remainingStems: 335, targetValue: 15,  targetVariable: "volume", targetRelative: true,  minDbh: 5,
				   classes: [30, 30, 30, 10, 0]
}


var thinning77a = { type: "thinning", 
				   schedule: { minRel: 0.69, optRel: 0.70, maxRel: 0.71 , force=true},
				   
                                   thinning: "custom",
				   onEvaluate: function() { console.log("7th THINNING ");return true;},
				   remainingStems: 265, targetValue: 15,  targetVariable: "volume", targetRelative: true,  minDbh: 5,
				   classes: [30, 30, 30, 10, 0]
}


var thinning7a_sw1 = { type: "thinning", 
				   schedule: { minRel: 0.79, optRel: 0.80, maxRel: 0.81 , force=true},
				   
                                 thinning: "custom",
				   onEvaluate: function() { console.log("8th THINNING sw");return true;},
				   targetValue: 40,  targetVariable: "volume", targetRelative: true,  minDbh: 5,
				   classes: [40, 30, 20, 5, 5]
}


var thinning7a_sw2 = { type: "thinning", 
				   schedule: { minRel: 0.89, optRel: 0.90, maxRel: 0.91 , force=true},
				   
                                 thinning: "custom",
				   onEvaluate: function() { console.log("9th THINNING sw");return true;},
				   targetValue: 60,  targetVariable: "volume", targetRelative: true,  minDbh: 5,
				   classes: [20, 20, 20, 20, 20]
}


///////////////////////////////////////           THINNING STP5 sw        //////////////////////////////////////////////////////

var thinning71b = { type: "thinning", 
				   schedule: { minRel: 0.09, optRel: 0.10, maxRel: 0.11 , force=true},
				   
                                   thinning: "custom",
				   onEvaluate: function() { console.log("1st THINNING ");return true;},
				    remainingStems: 705, targetValue: 25,  targetVariable: "volume", targetRelative: true,  minDbh: 0,
				   classes: [10, 20, 25, 25, 20]
}



var thinning72b = { type: "thinning", 
				   schedule: { minRel: 0.19, optRel: 0.20, maxRel: 0.21 , force=true},
				   
                                   thinning: "custom",
				   onEvaluate: function() { console.log("2nd THINNING ");return true;},
				   remainingStems: 505, targetValue: 25,  targetVariable: "volume", targetRelative: true,  minDbh: 5,
				   classes: [10, 20, 25, 25, 20]
}



var thinning73b = { type: "thinning", 
				   schedule: { minRel: 0.29, optRel: 0.30, maxRel: 0.31 , force=true},
				   
                                   thinning: "custom",
				   onEvaluate: function() { console.log("3rd THINNING ");return true;},
				   remainingStems: 405, targetValue: 20,  targetVariable: "volume", targetRelative: true,  minDbh: 5,
				   classes: [10, 20, 25, 25, 20]
}


var thinning74b = { type: "thinning", 
				   schedule: { minRel: 0.39, optRel: 0.40, maxRel: 0.41 , force=true},
				   
                                   thinning: "custom",
				   onEvaluate: function() { console.log("4th THINNING ");return true;},
				   remainingStems: 305, targetValue: 20,  targetVariable: "volume", targetRelative: true,  minDbh: 5,
				   classes: [10, 20, 25, 25, 20]
}


var thinning75b = { type: "thinning", 
				   schedule: { minRel: 0.49, optRel: 0.50, maxRel: 0.51 , force=true},
				   
                                   thinning: "custom",
				   onEvaluate: function() { console.log("5th THINNING ");return true;},
				   remainingStems: 255, targetValue: 15,  targetVariable: "volume", targetRelative: true,  minDbh: 5,
				   classes: [10, 20, 25, 25, 20]
}



var thinning76b = { type: "thinning", 
				   schedule: { minRel: 0.59, optRel: 0.60, maxRel: 0.61 , force=true},
				   
                                   thinning: "custom",
				   onEvaluate: function() { console.log("6th THINNING ");return true;},
				   remainingStems: 205, targetValue: 15,  targetVariable: "volume", targetRelative: true,  minDbh: 5,
				   classes: [30, 30, 30, 10, 0]
}


var thinning77b = { type: "thinning", 
				   schedule: { minRel: 0.69, optRel: 0.70, maxRel: 0.71 , force=true},
				   
                                   thinning: "custom",
				   onEvaluate: function() { console.log("7th THINNING ");return true;},
				   remainingStems: 155, targetValue: 10,  targetVariable: "volume", targetRelative: true,  minDbh: 5,
				   classes: [30, 30, 30, 10, 0]
}


var thinning7b_sw1 = { type: "thinning", 
				   schedule: { minRel: 0.79, optRel: 0.80, maxRel: 0.81 , force=true},
				   
                                 thinning: "custom",
				   onEvaluate: function() { console.log("8th THINNING sw");return true;},
				   targetValue: 40,  targetVariable: "volume", targetRelative: true,  minDbh: 5,
				   classes: [40, 30, 20, 5, 5]
}


var thinning7b_sw2 = { type: "thinning", 
				   schedule: { minRel: 0.89, optRel: 0.90, maxRel: 0.91 , force=true},
				   
                                 thinning: "custom",
				   onEvaluate: function() { console.log("9th THINNING sw");return true;},
				   targetValue: 60,  targetVariable: "volume", targetRelative: true,  minDbh: 5,
				   classes: [20, 20, 20, 20, 20]
}





//- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
//                                                      ---   C L E A R C U T   --->   



/// final harvest for all stands, specific dbh treshold can be set in the options of the stp
//  we use default 5 cm dbh threshold



// clearcut for STPs: we do NOT remove the regeneration thanks the tree.filter    https://iland-model.org/ABE+activity+scheduled?highlight=scheduler
// !! try to change scheduled in general !!

var finalcut =  { type: "scheduled",
			 schedule: { minRel: 0.99, optRel: 1.0, maxRel: 1.01, force=true },

			onEvaluate: function(){ 

	fmengine.log("finalHarvest:" + stand.activity.finalHarvest);
					stand.trees.loadAll();

	fmengine.log("---THE U WHAT iLAND USE HERE:" + stand.U);         // HAVE A LOOK AT "fmengine.log" HERE AND FOR THE FOLLOWING, IN THE XLXS FILE THE CODE IS A BIT DIFFERENT
	fmengine.log("Number of trees before selecting: "+ stand.trees.count);

					 stand.trees.filter("dbh>5")
					 stand.trees.filter("age>0")

	fmengine.log("finLcut: using age threshold: "+stand.U*(0) );
	fmengine.log("Number of trees after selecting: "+stand.trees.count);   // HERE THERE IS A KIND OF FUNCTION tree.count THAT CAN BE USED FOR THE TRASHOLD OF REMAINING TREES?! FOR THINNINIG

					stand.trees.harvest(); 
                                        return true;},
			onExecute: function() { 

         fmengine.log("I really do the harvest..."); 

					stand.trees.removeMarkedTrees(); 	
                 			},
					onCreate: function(act) {act.finalHarvest=true;  },
		onSetup: function() {  } 
					
};


//- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
////////////////////////////////////////////////////////////////////  STPS LIST ACTIVITIES  /////////////////////////////////////////////////

		
/// This show what activites are set for STP	
var stp1_sw = { U: [120,120,120], // short/normal/long rotation age --- are ignored if U is provided in input file  ... we have it in .csv
            options: {}, 
			planting_1_adp:  planting_1_adp, 
			
			salvager_sw: salvager_sw,
			thinning11: thinning11,
			thinning12: thinning12,
			thinning13: thinning13,
			thinning14: thinning14,
			thinning15: thinning15,
			thinning16: thinning16,
			thinning17: thinning17,
			thinning1_sw1: thinning1_sw1,
			thinning1_sw2: thinning1_sw2,
			finalcut:  finalcut,

            // standmonitoring: stand_monitor

      onInit: function() {}  
			
};



// register the stand treatment program stp with the name 'stp'
console.log('adding STPs...');
fmengine.addManagement(stp1_sw, 'STP1_SW');


//---------------------------------------------------------------------------------------------------------------------------------------
/// This show what activites are set for STP2	
var stp2_sw = { U: [100,100,100], // short/normal/long rotation age --- are ignored if U is provided in input file  ... we have it in .csv
            options: {}, 
			planting_3_adp:  planting_3_adp, 
			
			salvager_sw: salvager_sw,
			thinning31: thinning31,
			thinning32: thinning32,
			thinning33: thinning33,
			thinning34: thinning34,
			thinning35: thinning35,
			thinning36: thinning36,
			thinning37: thinning37,
			thinning3_sw1: thinning3_sw1,
			thinning3_sw2: thinning3_sw2,
			finalcut:  finalcut,

            // standmonitoring: stand_monitor

      onInit: function() {}  
			
};



// register the stand treatment program stp with the name 'stp'
console.log('adding STPs...');
fmengine.addManagement(stp2_sw, 'STP2_SW');


//---------------------------------------------------------------------------------------------------------------------------------------

/// This show what activites are set for STP3	
var stp3_sw = { U: [100,100,100], // short/normal/long rotation age --- are ignored if U is provided in input file  ... we have it in .csv
            options: {}, 
			planting_5_adp:  planting_5_adp, 
			
			salvager_sw: salvager_sw,
			thinning51: thinning51,
			thinning52: thinning52,
			thinning53: thinning53,
			thinning54: thinning54,
			thinning55: thinning55,
			thinning56: thinning56,
			thinning5_sw1: thinning5_sw1,
			thinning5_sw2: thinning5_sw2,
			thinning5_sw3: thinning5_sw3,
			finalcut:  finalcut,

            // standmonitoring: stand_monitor

      onInit: function() {}  
			
};



// register the stand treatment program stp with the name 'stp'
console.log('adding STPs...');
fmengine.addManagement(stp3_sw, 'STP3_SW');


//---------------------------------------------------------------------------------------------------------------------------------------

/// This show what activites are set for STP4	
var stp4_sw = { U: [100,100,100], // short/normal/long rotation age --- are ignored if U is provided in input file  ... we have it in .csv
            options: {}, 
			planting_7a_adp:  planting_7a_adp, 
			
			salvager_sw: salvager_sw,
			thinning71a: thinning71a,
			thinning72a: thinning72a,
			thinning73a: thinning73a,
			thinning74a: thinning74a,
			thinning75a: thinning75a,
			thinning76a: thinning76a,
			thinning77a: thinning77a,
			thinning7a_sw1: thinning7a_sw1,
			thinning7a_sw2: thinning7a_sw2,
			finalcut:  finalcut,

            // standmonitoring: stand_monitor

      onInit: function() {}  
			
};



// register the stand treatment program stp with the name 'stp'
console.log('adding STPs...');
fmengine.addManagement(stp4_sw, 'STP4_SW');

//---------------------------------------------------------------------------------------------------------------------------------------

/// This show what activites are set for STP5	
var stp5_sw = { U: [80,80,80], // short/normal/long rotation age --- are ignored if U is provided in input file  ... we have it in .csv
            options: {}, 
			planting_7b_adp:  planting_7b_adp, 
			
			salvager_sw: salvager_sw,
			thinning71b: thinning71b,
			thinning72b: thinning72b,
			thinning73b: thinning73b,
			thinning74b: thinning74b,
			thinning75b: thinning75b,
			thinning76b: thinning76b,
			thinning77b: thinning77b,
			thinning7b_sw1: thinning7b_sw1,
			thinning7b_sw2: thinning7b_sw2,
			finalcut:  finalcut,

            // standmonitoring: stand_monitor

      onInit: function() {}  
			
};



// register the stand treatment program stp with the name 'stp'
console.log('adding STPs...');
fmengine.addManagement(stp5_sw, 'STP5_SW');


//---------------------------------------------------------------    SCHEDULE AGENT TYPE        -------------------------------------------------------------------------------------------
// without doent's work well the model
// minimal agent type/            https://iland-model.org/ABE?highlight=minimal+agent+type        link 2 :    https://iland-model.org/ABE+agents
//  VERY IMPORTANT FOR THE BELOW CONSTRAINS UNDERSTANDING   https://iland-model.org/doxy/struct_a_b_e_1_1_scheduler_options.html

var base_agent = {
	scheduler: { 
		 enabled: true,
                 minScheduleHarvest: 1,
		 maxScheduleHarvest: 10,
		 scheduleRebounceDuration: 15,
		 maxHarvestLevel: 1.5,         // default: 1.5
		 deviationDecayRate: 0.1,      // default: 0.1
		 useSustainableHarvest: 0.6,   // 0 is bottom-up harvest planning (i.e., stands are always processed at their optimal dates), and 1 is top-down approach (i.e, the scheduling algorithm decides when a stand should be processed).
		 harvestIntensity: 0.5}, 

	
	
	
////////////// EDIT IT IF YOU WANT SWITCH STAND STPS WITH SOME TRASHOLD LIKE PROPORTION OF ONE SPECIES 
////////////// PAY ATTENTION 
////////////// a list of all STPs the agent has access to....
	
stp: { 'STP1_SW': 'STP1_SW' ,'STP2_SW': 'STP2_SW', 'STP3_SW': 'STP3_SW','STP4_SW': 'STP4_SW','STP5_SW': 'STP5_SW','default':'STP3_SW' },
newAgent: function() { return {  scheduler: this.scheduler }; },

        onSelect: function() { if (stand.flag("group")==1) {
              console.log(stand.flag("group")) ;
console.log('we should go back to STP1_SW');
return 'STP1_SW';
   }; 

if  (stand.flag("group")==2) {
console.log(stand.flag("group")) ;
console.log('we are in STP2_SW');
    return 'STP2_SW';
                    };

if (stand.flag("group")==3) {
              console.log(stand.flag("group")) ;
console.log('we should go back to STP3_SW');
return 'STP3_SW';                                    //  REMEMBER TO CHANGE IT WHILE YOU ADD OTHER STPS
}; 

if (stand.flag("group")==4) {
              console.log(stand.flag("group")) ;
console.log('we should go back to STP4_SW');
return 'STP4_SW';
}; 

if (stand.flag("group")==5) {
              console.log(stand.flag("group")) ;
console.log('we should go back to STP5_SW');
return 'STP5_SW';
}; 

if (stand.flag("group")==6) {
              console.log(stand.flag("group")) ;
console.log('we should go back to STP3_SW');
return 'STP3_SW';
};

},

run: function() { console.log('base-agent run called');
                         console.log(stand.relSpeciesBasalAreaOf('piab'))  }
};


console.log('adding Agents...');
// register the agent-factory object base_agent under the name agenttype
fmengine.addAgentType(base_agent, 'agenttype');
// use the 'agenttype' agent-factory and create an agent named 'agent'
fmengine.addAgent('agenttype', 'agent');

/*
// Writing out grids:   this is only to write out some grids (maps) of some info...

function onYearEnd()
{
 console.log("GlobalEvent: on year end: " + Globals.year);
if (Globals.year>=1 ) {
  writeGrids();
}
}



function writeGrids()
{

var gridname = Globals.setting('user.gridname'); // retrieve from XML


 BarkBeetle.gridToFile("sumVolume",  Globals.path('output') + '/' + gridname +'__bb_damage_' + Globals.year + '.asc' );
 Wind.gridToFile("sumVolume",  Globals.path('output') + '/' + gridname +'__wind_damage_' + Globals.year + '.asc' );

  
}
*/
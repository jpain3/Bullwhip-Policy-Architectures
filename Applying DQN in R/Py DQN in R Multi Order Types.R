#Using DQN Built in Python in R
#James Paine
#jpaine@mit.edu

####  SET UP OF WORKING DIRECTORY AND DEPENDENCIES####

##Set the working directory to the folder this script is located in
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

##Load the support function that actually runs the simulation game for a given timestep and action/state pair
source("PirateBeerGame_Opt_Function_ModelBased.R")

##Load, and install if necessary, any needed supporting packages
# Note that optimx can do all the optimizations needed, but here is subsituted for a 
# parallelized version, while still being used for axial improvement searches
list.of.packages <- c("tensorflow","keras")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if (length(new.packages)>0) {
  install.packages(new.packages)
}
lapply(list.of.packages, require, character.only = TRUE)

#An alternative series of commands to get keras and tensorflow up and running with the specific version needed here:
#install.packages("keras")
#reticulate::install_miniconda()
#keras::install_keras(version = "2.3.0")

#### Helper Functions ####

## function to write output to excel
write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}


## Helper Function to load parameters from a csv file ##
load_team_parameters <- function(filename = "JS Parameter Table.csv", Team_Index = NULL, Random_Team = FALSE, Random_Entities = FALSE ) {
  
  if (file.exists(filename)) {
    
    File_Not_Found = 0
    
    Parameter_df = read.csv(filename)
    
    #Repair blanks or zero values
    Parameter_df[is.na(Parameter_df)] = 0
    
    #Put index for a specific team here
    if (is.numeric(Team_Index)){
      Parameter_subdf = Parameter_df[Parameter_df$Team_Index==Team_Index,]
      TeamName = as.character(Parameter_df[Parameter_df$Team_Index==Team_Index,"Team_Name"][1])
      
    } else { #If the Team Index is NA or otherwise invalid, choose the default average parameters
      
      # #NOTE: Sterman 1989 Decision Heuristic Paramters
      # theta = 0.36
      # alpha_s = 0.26
      # beta = 0.34
      # S_prime = 17
      
      theta = rep(0.36,4)
      alpha_s = rep(0.26,4)
      beta = rep(0.34,4)
      S_prime = rep(17,4)
      TeamName = "Default JS Agents"
      
      Parameter_subdf=data.frame(theta,alpha_s,beta,S_prime)
      
    }
    
    if (Random_Team == TRUE) {
      Team_Index = sample(min(Parameter_df$Team_Index):max(Parameter_df$Team_Index), 1)
      Parameter_subdf = Parameter_df[Parameter_df$Team_Index==Team_Index,]
      TeamName = as.character(Parameter_subdf$Team_Name[1])
    }
    
    if (Random_Entities == TRUE) {
      
      for (q in 1:4) {
        
        Potential_entities = Parameter_df[Parameter_df$Entity_index==q,]
        Chosen_entity = Potential_entities[sample(1:nrow(Potential_entities),1),]
        
        Parameter_subdf[q,] = Chosen_entity
        TeamName = "Random Assortment of Teams"
        
      }
      
    }
    
    #Assign parameter values
    theta = unlist(Parameter_subdf$theta)
    alpha_s = unlist(Parameter_subdf$alpha_s)
    beta = unlist(Parameter_subdf$beta)
    S_prime = unlist(Parameter_subdf$S_prime)
    
  } else {
    
    #IF there is no input parameter file, load defaults:
    
    File_Not_Found = 1
    
    # #NOTE: Sterman 1989 Decision Heuristic Paramters
    # theta = 0.36
    # alpha_s = 0.26
    # beta = 0.34
    # S_prime = 17
    
    theta = rep(0.36,4)
    alpha_s = rep(0.26,4)
    beta = rep(0.34,4)
    S_prime = rep(17,4)
    
    TeamName = "Default JS Agents (Input File Not Found)"
    
  }
  
  Output_Parameter_df = data.frame(theta,alpha_s,beta,S_prime)
  
  FntOut = list(TeamName,Output_Parameter_df)
  names(FntOut) = c("TeamName","Par")
  
  return(FntOut)
  
}  

#### Set up order Stings ####
setup_orders <- function(order_seed = 123, Num_Draws = 50, horizon = 36,                       
                         Random_Orders = FALSE, Order_mean = 0, Order_SD = 1, 
                         Stationary_Orders = TRUE, Order_Slope = .5,
                         OrderType = "S89",
                         Step_Round = 5,        #Step_Round = NA #if NA then random step time is chosen
                         Step_Start = 4,        #OrderType = "CD06"
                         Step_End = 8,          #OrderType = "CD06"
                         UniformDrawMin = 0,    #OrderType = CD06
                         UniformDrawMax = 8,    #OrderType = CD06
                         NormalDrawMean = 10,   #OrderType = CS00
                         NormalDrawSD = 4       #OrderType = CS00 
)  {
  
  #### Set up Order Strings ####
  
  set.seed(order_seed)
  
  Global_Orders = array(dim=c(Num_Draws,horizon+1))
  
  for (i in (1:Num_Draws)) {
    
    if ((is.numeric(Order_Slope)) & (Stationary_Orders != TRUE)) {
      TimeSeriesX = seq(from = 0, to = horizon, by = 1)
      UnderlyingDrift = sapply(TimeSeriesX,function(x) {Order_Slope*x+0})
    } else {
      UnderlyingDrift = rep(0,horizon)
    }
    
    if (Random_Orders == TRUE){
      
      #Normally Distributed demand
      Order_mean = Order_mean
      Order_SD = Order_SD
      RandomOrders = rnorm(horizon+1,Order_mean, Order_SD)
      
    } else {
      RandomOrders = rep(0,horizon)
    }
    
    #Customer Order String
    
    if (OrderType == "S89") {
      
      #### STERMAN 1989 ####
      
      if (is.na(Step_Round)) {
        
        Step_Round = sample(seq(from = 1, to = horizon+1), size = 1)
        
      }
      
      ## Classic Beer Game step function ##
      Step_Round = max(0,Step_Round)
      RoundOrders = pmax(0,UnderlyingDrift + RandomOrders + 
                           append(rep(Step_Start,min(horizon+1,Step_Round)),
                                  rep(Step_End,max(0,((horizon+1)-Step_Round))))
      )
      
    }
    
    if (OrderType == "CD06") {
      
      #### CORSON AND DONOHUE 2006 ####
      
      ## Demand is uniform integer between 0 and 8
      
      RoundOrders = pmax(0,UnderlyingDrift + RandomOrders 
                         + sample(seq(from = UniformDrawMin, to = UniformDrawMax), size = horizon+1, replace = TRUE, prob = NULL))
      
    }
    
    if (OrderType == "CS00") {
      #### Chen and Samroengraja 2000 ####
      
      RoundOrders = pmax(0,UnderlyingDrift + RandomOrders + 
                           round(rnorm(n = horizon+1, mean = NormalDrawMean, sd = NormalDrawSD),0))
      
    }
    
    Global_Orders[i,] = RoundOrders
    
  } #next draw of the order sting
  
  return(Global_Orders)
  
  
} #end order function

####  RESET FUNCTION####
reset_game <- function(horizon = 36, Initial_Inventory = 12, Information_Delay = 2, Shipping_Delay = 2, 
                       Step_Round = 5, Step_Start = 4, Step_End = 8,
                       initial_orders = 4, initial_inbound = 4) {
  
  # Initial_Inventory = 12
  # horizon = 102
  # Information_Delay = 2
  # Shipping_Delay = 2
  
  
  
  ##################
  #Setting up the game
  ##################
  
  
  Order_flows = array(NA,dim = c(4,Information_Delay,(horizon+1)))
  #populate initial values of orders
  Order_flows[,,1] = initial_orders
  
  Shipping_flows = array(NA,dim = c(4,Shipping_Delay,(horizon+1)))
  #populate initial values of incomming shippments
  Shipping_flows[,,1] = initial_inbound
  
  OH_Inventory = array(NA,dim=c(4,(horizon+1)))
  OH_Inventory[,1] = Initial_Inventory
  
  Backorder = array(NA,dim=c(4,(horizon+1)))
  Backorder[,1] = 0
  
  L_hat = array(NA,dim=c(4,(horizon+1)))
  L = array(NA,dim=c(4,(horizon+1)))
  
  Final_Customer_Orders_Filled = rep(NA, (horizon+1))
  Production_Request = rep(NA,(horizon+1))
  Production_Request[1] = initial_orders
  
  Amp_Vector = rep(NA,horizon+1)
  Reward_Vector = rep(NA,horizon+1)
  
  Output <- list(Order_flows,Shipping_flows,OH_Inventory,Backorder,L_hat,L,
                 Final_Customer_Orders_Filled,Production_Request,
                 Amp_Vector, Reward_Vector)
  
  names(Output) = c("Order_flows", "Shipping_flows", "OH_Inventory", "Backorder", "L_hat", "L",
                    "Final_Customer_Orders_Filled", "Production_Request",
                    "Amp_Vector", "Reward_Vector")
  
  return(Output)
  
}

#### SIM PARAMETERS ####

Sim_horizon =52
Shipping_Delay = 2
Information_Delay = 2
Holding_Cost = 0.5
Backorder_Cost = 1
Initial_Inventory = 12

Integer_Ordering = FALSE

## Set customer order pattern

#OrderType = "S89"
#Step_Round = 5
#Step_Start = 4
#Step_End = 8

#Random_Orders = FALSE
#Order_mean = 0
#Order_SD = 1

#Stationary_Orders = TRUE
#Order_Slope = .5 #Only matters if above flag is FALSE


OrderType = "CS00"
NormalDrawMean = 10   #OrderType = CS00
NormalDrawSD = 4       #OrderType = CS00 


DNN_AI_Entity = TRUE

#Specify the pre-trained model file here
model_filename = "dqn_SmallObs" #This is a 'small' observation size (not model-based). Note that the min order here was -20
model_filename = "dqn_LargeObs" #This is a 'large' observation size (model-based). #Note that the min order here was -20

Relative_Min_Order = -20

DNN_AI_Entity_Index = 2 #Vary from 1 (Retailer)  to 4 (Factory)
                        # Note that DNN was trained on a (0:3) index here so this is adjusted below

#Supply_Chain_Parameter_File = "JS Parameter Table.csv"
#Team_Index_Vector = seq(0,11,by=1)

Supply_Chain_Parameter_File = "Combined Historic Fitted JS Parameter Table.csv"
Team_Index_Vector = seq(0,48,by=1)

# Note, if both set to 'FALSE' then the values from the team matching the Team_Index is used
Random_Team = FALSE      #If set to TRUE then an entire team will be randomly chosen from the 'JS Parameter Table' file
Random_Entities = FALSE  #If set to TRUE then a purely fictional team will be bootstrapped by combining individuals from the 'JS Parameter Table' file
PreSelectedBootstrapTeams = FALSE
Boostrap_Parameter_File = "Bootsrapped Parameter Table.csv"
NumBootsraps = 100



#### MAIN LOOP ####

# initialize history vectors
DNN_Agent_Performance_List = list()
No_Agent_Performance_List=list()

## Load a saved and Trained Model if Requested##
if (DNN_AI_Entity == TRUE) {
  AI_Entity = TRUE
  AI_Entity_Index = DNN_AI_Entity_Index
} else {
  AI_Entity = FALSE
  AI_Entity_Index = FALSE
}

if (DNN_AI_Entity == TRUE) {
  
  agent = keras$models$load_model(model_filename)
  
  #Get the implied window size used when originally training the loaded model:
  model_input_shape = agent$get_layer(index = as.integer(0))$output_shape[[1]]  #Get the shape attribute from the input layer
  Original_Batch_Size = model_input_shape[[1]]                                  #First number is the number of items looked as simulateously
  Original_Window_Size = model_input_shape[[2]]                                 #Second number is the window used for any sequential memory
  Original_Observation_Size = model_input_shape[[3]]                            #Third number and (and onwards for multi dimensional inputs) is the actual observed space
  
  
  #"Small" is the smaller 6 observation points for a true model-free approach
  #"Large" is the larger 6+12 = 16 observation points that include the estimate of the other players parameters
  if (Original_Observation_Size == 6) {obs_size = "Small"}
  if (Original_Observation_Size == 18) {obs_size = "Large"}
  
  if (obs_size == "Small") {
    obs = c(4, 12, 0,
          4, 0, (AI_Entity_Index-1))
  }
  
  if (obs_size == "Large") {
    
    #Get the initial observation space parameters
    SupplyChainParameters = load_team_parameters(filename = Supply_Chain_Parameter_File, 
                                                 Team_Index = NA, 
                                                 Random_Team = Random_Team, 
                                                 Random_Entities=Random_Entities)
    
    #Trim away the line corresponding to where the agent acts
    trimmed_thetas = as.numeric(SupplyChainParameters$Par$theta[-AI_Entity_Index])
    trimmed_alphas = as.numeric(SupplyChainParameters$Par$alpha_s[-AI_Entity_Index])
    trimmed_betas = as.numeric(SupplyChainParameters$Par$beta[-AI_Entity_Index])
    trimmed_S_primes = as.numeric(SupplyChainParameters$Par$S_prime[-AI_Entity_Index])
    
    OtherPlayer1_theta = trimmed_thetas[1]
    OtherPlayer2_theta = trimmed_thetas[2]
    OtherPlayer3_theta = trimmed_thetas[3]
    OtherPlayer1_alphas_s = trimmed_alphas[1]
    OtherPlayer2_alphas_s = trimmed_alphas[2]
    OtherPlayer3_alphas_s = trimmed_alphas[3]
    OtherPlayer1_beta = trimmed_betas[1]
    OtherPlayer2_beta = trimmed_betas[2]
    OtherPlayer3_beta = trimmed_betas[3]
    OtherPlayer1_S_prime = trimmed_S_primes[1]
    OtherPlayer2_S_prime = trimmed_S_primes[2]
    OtherPlayer3_S_prime = trimmed_S_primes[3]
    
    #Assemble the full observation space
    obs = c(4, 12, 0,
            4, 0, (AI_Entity_Index-1),
            OtherPlayer1_theta,
            OtherPlayer2_theta,
            OtherPlayer3_theta,
            OtherPlayer1_alphas_s,
            OtherPlayer2_alphas_s,
            OtherPlayer3_alphas_s,
            OtherPlayer1_beta,
            OtherPlayer2_beta,
            OtherPlayer3_beta,
            OtherPlayer1_S_prime,
            OtherPlayer2_S_prime,
            OtherPlayer3_S_prime)
  }
  
  # Extract the initial steady-state received from the observations set for use in the first instance of relative ordering
  Agent_Order_Received = obs[1] ##RETURN TO THIS! Might be error in DQN Python Code
  
  #Expand initial observation out to fill history or window length
  obs = matrix(rep(obs,Original_Window_Size),nrow=Original_Window_Size, ncol=length(obs), byrow=TRUE)
  
}

if (PreSelectedBootstrapTeams == TRUE) {
  Team_Index_Vector = seq(1,NumBootsraps,by=1)
}


#Pre-generate the order strings
ResetHorizon = Sim_horizon
Global_Orders = setup_orders(OrderType = OrderType, horizon = ResetHorizon, Num_Draws = max(Team_Index_Vector+1), NormalDrawMean= NormalDrawMean, NormalDrawSD=NormalDrawSD)


##Debug
Team_Index = 0

#START LOOP OVER TEAMS
for (Team_Index in Team_Index_Vector) {

  #Get the order sting for this team
  Orders = as.vector(Global_Orders[Team_Index + 1,])

  #Import the relevant ordering parameters
  if (PreSelectedBootstrapTeams != TRUE) {
    
    SupplyChainParameters = load_team_parameters(filename = Supply_Chain_Parameter_File, 
                                                 Team_Index = Team_Index, 
                                                 Random_Team = Random_Team, 
                                                 Random_Entities=Random_Entities)
  } else {
    
    SupplyChainParameters = load_team_parameters(filename = Boostrap_Parameter_File, 
                                                 Team_Index = Team_Index, 
                                                 Random_Team = FALSE, 
                                                 Random_Entities=FALSE)
    
  }
  
  #Set the ordering Parameters of the other players:
  Parameter_df = as.data.frame(SupplyChainParameters$Par)
  
  ResetHorizon = Sim_horizon
  Reset_List = reset_game(Shipping_Delay = Shipping_Delay, Information_Delay = Information_Delay, 
                          Initial_Inventory = Initial_Inventory, horizon = ResetHorizon)
  list2env(Reset_List, envir = .GlobalEnv)
  
  ##debug
  t= 2
  
  for (t in 2:(Sim_horizon+1)) {
    
    if (DNN_AI_Entity == TRUE) {
      # Coerce the 1-D or 2-D observation input into a 2-D or 3-D array that TensorFlow will flatten and accept
      #resized_obs = obs[np.newaxis, ...]
      #need to go from size of (4,6) to size of (1,4,6)
      
      resized_obs = as.array(obs)
      dim(resized_obs) = c(1,dim(obs)[1], dim(obs)[-1])
      
      # Make AI ordering decision based on game state
      #qmatrix = agent.predict(resized_obs)
      qmatrix = agent(resized_obs)
      flattened_q = as.numeric(qmatrix)
      BestChoice = which.max(flattened_q)
      
      Relative_Order = (BestChoice - 1) + Relative_Min_Order  # need to subtract 1 as R indexes from 1 and not 0
      Agent_Order = max(0, Agent_Order_Received + Relative_Order)
      
      AI_Order = Agent_Order
      }else{
        AI_Order = FALSE
      }
    
    # NESTED FUNCTION TO RUN THE GAME FOR ONE TIME STEP AND RETURN THE NEW STATE
    R_function_output = PirateBeerGame_Opt(AI_Entity = AI_Entity, AI_Entity_Index=AI_Entity_Index, 
                                     AI_parameter_df = FALSE, 
                                     AI_Order = Agent_Order, 
                                     t = t, 
                                     Orders, 
                                     Order_flows, 
                                     Shipping_flows, 
                                     OH_Inventory, 
                                     Backorder,
                                     L, L_hat, Production_Request,
                                     Parameter_df = Parameter_df, 
                                     Integer_Ordering = Integer_Ordering, 
                                     Noisey_Ordering = FALSE,
                                     Shipping_Delay = Shipping_Delay, Information_Delay = Information_Delay)
    
    #Write the ouput of that function to the same variables that will be used for
    # its input in the next time step
    Order_flows = R_function_output$Order_flows
    Shipping_flows = R_function_output$Shipping_flows
    OH_Inventory = R_function_output$OH_Inventory
    Backorder = R_function_output$Backorder
    L = R_function_output$L
    L_hat = R_function_output$L_hat
    Production_Request = R_function_output$Production_Request
    Order_Received = R_function_output$Order_Received
    Reward_Vector[t]=R_function_output$reward
    Order_Received = R_function_output$Order_Received
    
    #Update the observation for the DNN AI
    #Observed_State = np.array([Agent_Order_Received, Agent_OH_Inventory, Agent_Backorder,
    #                          Agent_Recent_Order, period, AI_Entity_Index])
    Agent_Order_Received = Order_Received[AI_Entity_Index]
    Agent_OH_Inventory =OH_Inventory[AI_Entity_Index]
    Agent_Backorder = Backorder[AI_Entity_Index]
  
    if (AI_Entity_Index == 4){
      Agent_Recent_Order = Production_Request[t]
    }else{
      Agent_Recent_Order = Order_flows[AI_Entity_Index + 1, 1,t]
    }
    
    AI_Entity_Index = AI_Entity_Index
    
    if (DNN_AI_Entity == TRUE) {
      
      #AI Observation for this specific time step
      if (obs_size == "Small"){
        
        Observed_State = c(Agent_Order_Received, Agent_OH_Inventory, Agent_Backorder,
                                   Agent_Recent_Order, (t-1), (AI_Entity_Index-1))
      }
      
      if (obs_size == "Large"){
        
        #Trim away the line corresponding to where the agent acts
        trimmed_thetas =  as.numeric(Parameter_df$theta[-AI_Entity_Index])
        trimmed_alphas =  as.numeric(Parameter_df$alpha_s[-AI_Entity_Index])
        trimmed_betas =  as.numeric(Parameter_df$beta[-AI_Entity_Index])
        trimmed_S_primes =  as.numeric(Parameter_df$S_prime[-AI_Entity_Index])
        
        OtherPlayer1_theta = trimmed_thetas[1]
        OtherPlayer2_theta = trimmed_thetas[2]
        OtherPlayer3_theta = trimmed_thetas[3]
        OtherPlayer1_alphas_s = trimmed_alphas[1]
        OtherPlayer2_alphas_s = trimmed_alphas[2]
        OtherPlayer3_alphas_s = trimmed_alphas[3]
        OtherPlayer1_beta = trimmed_betas[1]
        OtherPlayer2_beta = trimmed_betas[2]
        OtherPlayer3_beta = trimmed_betas[3]
        OtherPlayer1_S_prime = trimmed_S_primes[1]
        OtherPlayer2_S_prime = trimmed_S_primes[2]
        OtherPlayer3_S_prime = trimmed_S_primes[3]
        
        #Assemble the full observation space
        Observed_State = c(Agent_Order_Received, Agent_OH_Inventory, Agent_Backorder,
                            Agent_Recent_Order, (t-1), (AI_Entity_Index-1),
                            OtherPlayer1_theta,
                            OtherPlayer2_theta,
                            OtherPlayer3_theta,
                            OtherPlayer1_alphas_s,
                            OtherPlayer2_alphas_s,
                            OtherPlayer3_alphas_s,
                            OtherPlayer1_beta,
                            OtherPlayer2_beta,
                            OtherPlayer3_beta,
                            OtherPlayer1_S_prime,
                            OtherPlayer2_S_prime,
                            OtherPlayer3_S_prime)
      
      }
      
    
    }
    
    #Assemble Full AI observation, including historic window
    if (Original_Window_Size > 1){
      HistoricObs = obs[1:(Original_Window_Size-1),]
      obs[1,] = Observed_State
      obs[2:Original_Window_Size,] = HistoricObs
    }else{
      obs = Observed_State
    }
    
  } #Next t
  
  
  #Calculate costs
  Costs_Per_Period = OH_Inventory*Holding_Cost + Backorder*Backorder_Cost
  Cummulative_Costs_Per_Period = t(apply( Costs_Per_Period ,1 , cumsum))
  Total_Costs_Per_Entity = rowSums(Costs_Per_Period,na.rm = TRUE)
  Total_Team_Costs = sum(Total_Costs_Per_Entity, na.rm = TRUE)
  
  #Calculate reward based on amplitude function
  total_amp_reward = sum(Reward_Vector, na.rm=TRUE)
  
  #Determine orders placed per period
  Final_Orders = unname(rbind(Order_flows[2:4,Information_Delay,],Production_Request))
  
  HistoryList = list(OH_Inventory,Backorder,Final_Orders,Total_Team_Costs)
  names(HistoryList) = c("OH_Inventory","Backorder","Final_Orders","Total_Team_Costs")
  
  #Save historic values for later comparison
  if (DNN_AI_Entity==TRUE){
    DNN_Agent_Performance_List[[as.character(Team_Index)]] = HistoryList
  } else {
    No_Agent_Performance_List[[as.character(Team_Index)]] = HistoryList
  }
  
} #next team

#### OUTPUTS AND REPORTS ####

##Combo Objects

if (DNN_AI_Entity==TRUE){
  OutputList = DNN_Agent_Performance_List
  ColumnName = paste("DNN Agent (",obs_size," obs size)",sep="")
} else {
  OutputList=No_Agent_Performance_List
  ColumnName = "Baseline"
}

Combo_Reward_Array = array(NA,dim=c(length(OutputList),1))
rownames(Combo_Reward_Array) = names(OutputList)
colnames(Combo_Reward_Array) = ColumnName

for (i in 1:length(OutputList)) {
  Combo_Reward_Array[i,1] = OutputList[[i]]$Total_Team_Costs
}


write.excel(Combo_Reward_Array)

Combo_Reward_Array

#### MODEL-PREDICTIVE LEARNING AGENT ####


#This script performs the model-predictive control as defined in SET EXPERIMENT PARAMETERS section below
#For example, as parameterized here, this agent has the following features:
#*Underlying environment based on Sterman '89 paper with parameters drawn from the supporting 'Combined Historic Fitted JS Parameter Table.csv' file
#*Agent has a Base-Stock model of the environment
#*Agent has a Base-Stock response
#*The order input is normally drawn based on Chen & Samroengraja 2009
#*The agent has a forward optimization horizon of 30 units of time
#*The agent has a backward calibration memory of 10 units of time and checks the fit every time step

#As configured here, this script writes to a file called 'BackupTemp.csv' after completing each experiment


####  SET UP OF WORKING DIRECTORY AND DEPENDENCIES####

##Set the working directory to the folder this script is located in
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

##Load the support function that actually runs the simulation game for a given timestep and action/state pair
source("PirateBeerGame_Opt_Function.r")

##Load, and install if necessary, any needed supporting packages
# Note that optimx can do all the optimizations needed, but here is subsituted for a 
# parallelized version, while still being used for axial improvement searches
list.of.packages <- c("optimParallel","optimx","profvis")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if (length(new.packages)>0) {
  install.packages(new.packages)
}
lapply(list.of.packages, require, character.only = TRUE)

####  HELPER FUNCTIONS####

#Helper function to write output to excel
write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}

#Helper function to write percents
percent <- function(x, digits = 2, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}

#Helper Function to insert a row in to a dataframe
insertRow <- function(existingDF, newrow, r) {
  
  if (r>nrow(existingDF)){
    existingDF[r,]<-newrow
  } else {
    existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
    existingDF[r,] <- newrow
  }
  existingDF
  
}

#Helper Function to load parameters from a csv file
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


####  OPTIMIZATION AND CALIBRATION MAIN FUNCTION####

#Note: For the forward-looking optimization, this is the function that is optimized over
# However for the backward looking calibration, this serves as a subfunction to the full
# calibration function defined below.

BeerGame_Optimization_with_History <- function(Agent_Parameter = FALSE, Agent_Parameter_History = FALSE, Agent_Order_History = FALSE, AI_Entity_Index = FALSE, 
                                               BaseStockAgent = FALSE, GreedyStepAgent = FALSE, Integer_Ordering = FALSE, 
                                               Sterman89 = TRUE, Oliva21 = FALSE, OlivaModel = "Model3", StermanDogan15 = FALSE,
                                               BaseStock = FALSE, Safety_Stock = NA,
                                               start_time = 1, horizon = 36, Parameter_df = FALSE, Orders,
                                               Holding_Cost, Backorder_Cost, Reward = "Cost", Mode = "Optimization",
                                               Myopic_Reward = FALSE, Selfweight = 1) {
  
  #If BaseStock == TRUE then Parameter_df must be ignored (eg == FALSE) and Agent_Parameter_History is a vector of single numbers
  
  
  #Note two possible modes "Optimization" or "Calibration"
  #For Optimization, only the reward of the function is returned
  #For calibration, the full state space of the system is returned
  
  #If optimizing and/or calibrating, it is assumed that there is an agent being 
  # optimzied or calibrated, and it's decision is dynamic, not static.
  
  if (Agent_Parameter[1] != FALSE || Oliva21 == TRUE || StermanDogan15 == TRUE ) {
    AI_Entity = TRUE
  } else {
    AI_Entity = FALSE
  }
  
  AI_Entity = TRUE
  
  #For the greedy agent, we assume the order is static and known in the past
  if (GreedyStepAgent != TRUE) {
    AI_Order = FALSE
  } else {
    AI_Order = TRUE
  }
  
  
  ###NEW###
  #If both the agent is Base Stock and the Environment is Basestock then ensure that the Agent's base-stock value is included in the full Safety_Stock vector
  if ((BaseStockAgent == TRUE) & (BaseStock == TRUE)) {
    
    if (length(Safety_Stock) == 4){
      if ((is.numeric(Agent_Parameter)) & (length(Agent_Parameter) == 1)) {
        Safety_Stock[AI_Entity_Index] = Agent_Parameter
      }
      
    } else if (length(Safety_Stock) == 3) {
      if ((is.numeric(Agent_Parameter)) & (length(Agent_Parameter) == 1)) {
        Safety_Stock[AI_Entity_Index] = append(Safety_Stock,Agent_Parameter,after=(AI_Entity_Index-1))
      }
    }
    
  } else if (BaseStockAgent == TRUE) {
    #Populate a dummy vector to satisfy the other routines
    if (length(Safety_Stock) == 1) {
      Safety_Stock = rep(Safety_Stock,4)
    }
  }
  
  if (BaseStock == TRUE) {
    Parameter_df = FALSE
  }
  
  
  
  ###/NEW###
  
  #To avoid throw an error in the correct spot if these critical parameters are not specified:
  AI_Entity_Index = AI_Entity_Index
  horizon = horizon
  
  #Initialize empty state value arrays for calibration mode
  if (Mode == "Calibration"  || Mode == "Outputs") {
    
    Order_flows_cal = array(NA,horizon+1)
    Shipping_flows_cal = array(NA,horizon+1)
    OH_Inventory_cal = array(NA,horizon+1)
    Backorder_cal = array(NA,horizon+1)
    Production_Request_cal = array(NA,horizon+1)
    
    
  }
  
  #Iterate over the horizon of interest.
  for (t in 2:(horizon+1)) {
    
    #If before the 'start time' then use the history of the agent's ordering rules to simulate the past behavior
    # Otherwise, use the current best guess for the agent's cost reducing ordering rule
    # Note that this becomes moot for calibration, where the the start time is the horizon
    
    if (AI_Entity_Index != FALSE) {
      
      if (t<start_time) {
        
        #Populate based on history
        
        if (GreedyStepAgent != TRUE) {
          
          if (BaseStockAgent != TRUE) {
            AI_parameter_df = Agent_Parameter_History[,t]
            names(AI_parameter_df) = c("theta","alpha_s","beta","S_prime")
          } else {
            AI_parameter_df = FALSE
            Safety_Stock[AI_Entity_Index] = Agent_Parameter_History[t]
          }
          
          
        } else {
          AI_Order = Agent_Order_History[t]
          AI_parameter_df = FALSE
        }
        
      } else {
        
        #Populate based on future
        
        if (GreedyStepAgent != TRUE) {

          if (BaseStockAgent != TRUE) {
            AI_parameter_df =Agent_Parameter
            names(AI_parameter_df) = c("theta","alpha_s","beta","S_prime")
          } else {
            AI_parameter_df = FALSE
            Safety_Stock[AI_Entity_Index] = Agent_Parameter
          }

        } else {
          AI_Order = Agent_Parameter[t] #Note that 'Agent Parameter' in the step-by-step greedy case is actually a vector of orders over the forward horizon
          AI_parameter_df = FALSE
        }
      }
      
    } else {
      AI_parameter_df = FALSE
    }
    

    #Call the external function that takes the state of the system, simulates 
    # for one time step, and returns and updated state space
    R_function_output = PirateBeerGame_Opt(AI_Entity, AI_Entity_Index = AI_Entity_Index, BaseStockAgent = BaseStockAgent, 
                                           AI_parameter_df = AI_parameter_df, AI_Order = AI_Order,
                                           Integer_Ordering = Integer_Ordering,
                                           Sterman89 = Sterman89, Oliva21 = Oliva21, OlivaModel = OlivaModel, StermanDogan15 = StermanDogan15,
                                           BaseStock = BaseStock, Safety_Stock = Safety_Stock,
                                           t = t, Orders = Orders,
                                           Order_flows = Order_flows, Shipping_flows = Shipping_flows, OH_Inventory = OH_Inventory, Backorder = Backorder,
                                           L = L, L_hat = L_hat, Production_Request = Production_Request,
                                           Parameter_df = Parameter_df)
    
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
    
  } #next t
  
  #If mode is calibration, store the observed state values
  if (Mode == "Calibration" || Mode == "Outputs") {
    
    Order_flows_cal = R_function_output$Order_flows
    Shipping_flows_cal = R_function_output$Shipping_flows
    OH_Inventory_cal = R_function_output$OH_Inventory
    Backorder_cal = R_function_output$Backorder
    Production_Request_cal = R_function_output$Production_Request
    Order_Received_cal = R_function_output$Order_Received
    
  }
  
  #Calculate costs
  Costs_Per_Period = OH_Inventory*Holding_Cost + Backorder*Backorder_Cost
  Cummulative_Costs_Per_Period = t(apply( Costs_Per_Period ,1 , cumsum))
  Total_Costs_Per_Entity = rowSums(Costs_Per_Period,na.rm = TRUE)
  Total_Team_Costs = sum(Total_Costs_Per_Entity, na.rm = TRUE)
  
  Trimmed_Costs_Per_Entity = rowSums(Costs_Per_Period[,max(1,start_time):(dim(Costs_Per_Period)[2])],na.rm = TRUE)
  Trimmed_Total_Team_Costs = sum(Trimmed_Costs_Per_Entity, na.rm = TRUE)
  
  if (Myopic_Reward == TRUE) {
    
    AI_Entity_Costs = Total_Costs_Per_Entity[AI_Entity_Index]
    NonAI_Costs = Total_Team_Costs - AI_Entity_Costs
    Weighted_Myopic_Total_Costs = Selfweight*AI_Entity_Costs + (1-Selfweight)*NonAI_Costs
    
    Trimmed_AI_Entity_Costs = Trimmed_Costs_Per_Entity[AI_Entity_Index]
    Trimmed_NonAI_Costs = Trimmed_Total_Team_Costs - Trimmed_AI_Entity_Costs
    Trimmed_Weighted_Myopic_Total_Costs = Selfweight*Trimmed_AI_Entity_Costs + (1-Selfweight)*Trimmed_NonAI_Costs
    
  }
  
  #Calculate reward based on amplitude function
  total_amp_reward = sum(Reward_Vector, na.rm=TRUE)
  Trimmed_total_amp_reward = sum(Reward_Vector[max(1,start_time):length(Reward_Vector)], na.rm=TRUE)
  
  #If in calibration mode, return the full set of observations from the simulation
  # Otherwise, assumed to be in optimization mode and returns the reward value
  if (Mode == "Calibration" || Mode == "Outputs"){
    
    if (Reward == "Amp") {
      SimReward = total_amp_reward
    } else {
      SimReward = Total_Team_Costs
    }
    
    Cal_output = list(Order_flows_cal, Shipping_flows_cal, OH_Inventory_cal, Backorder_cal,
                      Production_Request_cal,Order_Received_cal,SimReward)
    
    names(Cal_output) = c("Order_flows", "Shipping_flows", "OH_Inventory", "Backorder",
                          "Production_Request","Order_Received","Reward")
    
    return(Cal_output)
    
  } else{
    #Two different rewards for the optimization are assumed to be possible:
    # 1. A reduction of the defintion of amplification factor (defined in the nested
    #     subfunction above)
    # 2. The more traditional and straight-foward total costs incurred by the
    #     supply chain
    if (Reward == "Amp") {
      return(Trimmed_total_amp_reward)
    } else {
      if (Myopic_Reward == TRUE) {
        return(Trimmed_Weighted_Myopic_Total_Costs)
      } else {
        return(Trimmed_Total_Team_Costs)
      }
    }
  }
  
} #End optimization function

####  CALIBRATION WRAPPER FUNCTION####
CalibrationFunction <- function(Parameter_Vector_Estimate,True_Parameter_df,
                                CalWeights,CalibrationType,Agent_Parameter_Guess,
                                Agent_Parameter_History = FALSE,
                                Agent_Order_History = FALSE,
                                AI_Entity_Index,
                                Holding_Cost,
                                Backorder_Cost,
                                Integer_Ordering = FALSE,
                                BaseStockAgent = FALSE,
                                GreedyStepAgent = FALSE,
                                Orders,
                                Sterman89 = TRUE, Oliva21 = FALSE, OlivaModel = "Model3", StermanDogan15 = FALSE,
                                BaseStock = FALSE, Safety_Stock = NA,
                                EstSterman89 = TRUE, EstOliva21 = FALSE, EstOlivaModel = "Model3", EstStermanDogan15 = FALSE,
                                EstBaseStock = FALSE, EstSafety_Stock = NA,
                                Calibration_Start,
                                Calibration_Horizon){
  
  
  #Extract the variables being varied in the calibration and reassemble them 
  # into a dataframe as is expected by other subfunctions
  
  ##DEBUG
  #if (Calibration_Horizon >=5){
    #Calibration_Horizon = Calibration_Horizon
  #}
  
  if (GreedyStepAgent == TRUE) {
    AgentParameters = rep(NA,4)
  } else {
    
    if (BaseStockAgent == TRUE) {
      AgentParameters = Agent_Parameter_History[Calibration_Horizon]
    } else {
      AgentParameters = Agent_Parameter_History[,Calibration_Horizon]
    }
  }

  if ((BaseStock == TRUE) | (EstBaseStock == TRUE)) {
    Parameter_df_Estimate = FALSE
    EstSafety_Stock = Parameter_Vector_Estimate
    
  } else {
    Base_Parameters_without_Agent = as.data.frame(array(Parameter_Vector_Estimate, dim = c(3,4)))
    Parameter_df_Estimate = insertRow(Base_Parameters_without_Agent,AgentParameters,AI_Entity_Index)
    names(Parameter_df_Estimate) = c("theta","alpha_s","beta","S_prime")
  }
  
  #Simulate behavior based on the estimated order parameters of the other agents and the agent actions so far
  Calibration_Estimate = BeerGame_Optimization_with_History(Agent_Parameter = Agent_Parameter_Guess, 
                                                            start_time = Calibration_Horizon, 
                                                            Agent_Parameter_History = Agent_Parameter_History,
                                                            Agent_Order_History = Agent_Order_History,
                                                            AI_Entity_Index = AI_Entity_Index,
                                                            BaseStockAgent = BaseStockAgent,
                                                            GreedyStepAgent = GreedyStepAgent,
                                                            Holding_Cost = Holding_Cost,
                                                            Backorder_Cost = Backorder_Cost,
                                                            Integer_Ordering = Integer_Ordering,
                                                            Sterman89 = EstSterman89, Oliva21 = EstOliva21, OlivaModel = EstOlivaModel,
                                                            StermanDogan15 = EstStermanDogan15, #KEY DIFFERENCE: Here we are assuming the other players all use the Sterman '89 model
                                                            BaseStock = EstBaseStock, Safety_Stock = EstSafety_Stock,
                                                            horizon = Calibration_Horizon,
                                                            Orders = Orders,
                                                            Parameter_df = Parameter_df_Estimate, #KEY DIFFERENCE: Here we are using our best estimate of the other entities ordering heuristics
                                                            Mode = "Calibration")
  
  #Recreate the behavior based on the *GROUND TRUTH* order parameters of the other agents and the agent actions so far
  # Note that another method for this would be to keep track of the history with each step of the master time loop
  Calibration_Reality = BeerGame_Optimization_with_History(Agent_Parameter = Agent_Parameter_Guess, 
                                                           start_time = Calibration_Horizon, 
                                                           Agent_Parameter_History = Agent_Parameter_History,
                                                           Agent_Order_History = Agent_Order_History,
                                                           AI_Entity_Index = AI_Entity_Index,
                                                           BaseStockAgent = BaseStockAgent,
                                                           GreedyStepAgent = GreedyStepAgent,
                                                           Holding_Cost = Holding_Cost,
                                                           Backorder_Cost = Backorder_Cost,
                                                           Integer_Ordering = Integer_Ordering,
                                                           Sterman89 = Sterman89, Oliva21 = Oliva21, OlivaModel = OlivaModel, 
                                                           StermanDogan15 = StermanDogan15, #KEY DIFFERENCE: Here we using the actual model the other players use
                                                           BaseStock = BaseStock, Safety_Stock = Safety_Stock,
                                                           horizon = Calibration_Horizon, 
                                                           Orders = Orders,
                                                           Parameter_df = True_Parameter_df, #KEY DIFFERENCE: Here we are using the actual ordering heuristics of the other entities
                                                           Mode = "Calibration")
  
  #Note that the Calibration simulation outputs a list object with the 
  #   following state values over full calibration horizon:
  #   "Order_flows", "Shipping_flows", "OH_Inventory", "Backorder", "Production_Request"
  
  #Now we trim those observations down to just the calibration window
  
  if (Calibration_Horizon == 5) {
    Calibration_Horizon = Calibration_Horizon
  }
  
  #Reset the calibration error and recalculate weighted mean squared residual error
  ErrorTotal = 0
  for (i in (1:length(Calibration_Estimate))) {
    
    #Check to see if there is a weight defined for the specific state variable
    #   Assume that if no weight is defined then the user does not want to use
    #   that in the observation space of the agent
    if (is.na(CalWeights[names(Calibration_Estimate)[i]])){
      
    } else {
      
      #Match the weights to the item being calibrated over
      Weight = CalWeights[names(Calibration_Estimate)[i]]
      
      Data_Reality = Calibration_Reality[[i]]
      Data_Estimate = Calibration_Estimate[[i]]
      
      #First check for the special case of CalibrationType == "Observed_Only" in which
      # it is assumed that the agent can only see the orders itself received and no others
      
      if ((CalibrationType == "Observed_Only") & (names(Weight) == "Order_flows")){
        #For order flows, observed only is only the actually observed order, not the full chain
        Data_Reality = Data_Reality[AI_Entity_Index,1,]
        Data_Estimate = Data_Estimate[AI_Entity_Index,1,]
      }
      
      if ((CalibrationType == "Small_Observed_Only") & (names(Weight) == "Order_flows")){
        #For order flows, observed only is only the actually observed order, not the full chain
        Data_Reality = Data_Reality[AI_Entity_Index,1,]
        Data_Estimate = Data_Estimate[AI_Entity_Index,1,]
      }
      
      if ((CalibrationType == "Small_Observed_Only") & (names(Weight) == "Shipping_flows")){
        #For shipping flows, for very small observed only is the full inbound shipments in transit
        Data_Reality = Data_Reality[AI_Entity_Index,,]
        Data_Estimate = Data_Estimate[AI_Entity_Index,,]
      }
      
      if ((CalibrationType == "Small_Observed_Only") & (names(Weight) == "OH_Inventory")){
        #For OH Inventory, for very small observed only is ONLY the inventory of the agent
        Data_Reality = Data_Reality[AI_Entity_Index,]
        Data_Estimate = Data_Estimate[AI_Entity_Index,]
      }
      
      
      #Determine the point-by point error
      PointErrors = Weight*((Data_Reality - Data_Estimate)^2)
      
      #Determine which element of the array is the time dimension
      # Assumed to be the last dimension of the array
      SumAxis = max(1,length(dim(PointErrors)))
      
      #Trim and then Sum along the time dimension
      if (SumAxis == 1) {
        
        if (Calibration_Start>1){
          PointErrors = tail(PointErrors,-(Calibration_Start-1))
        }
        ErrorSum = sum(PointErrors, na.rm = TRUE)
        
      } else {
        
        if (Calibration_Start>1){
          ParameterTimeSteps = dim(PointErrors)[SumAxis]
          if (SumAxis==3){
            PointErrors = PointErrors[, , seq(from = Calibration_Start, to=ParameterTimeSteps)]
          } else {
            PointErrors = PointErrors[, seq(from = Calibration_Start, to=ParameterTimeSteps)]
          }
        }
        
        ErrorSum = rowSums(PointErrors, na.rm = TRUE, dims = SumAxis-1)
      }
      
      #Above would technically allow you to assign weights based on individual position and time
      # For simplicity here, this is all merged together
      
      ErrorTotal = ErrorTotal + sum(ErrorSum,  na.rm = TRUE)
      
    } #End check of if weights exist to calibrate against
    
  }
  
  return(ErrorTotal)
  
} #End Calibration Function

####  SET EXPERIMENT PARAMETERS####

## Set the agent acting in this system (between 1 and 4)
AI_Entity_Index = 2 #2 = Wholesaler

#Set how Myopic the agent is
Myopic_Reward = TRUE    #IF false, the agent considers the full team cost when optimizing
Selfweight = 1          #If agent is Myopic, it can be totally Myopic with a weight of 1, or totally 'selfless' with a weight of 0.

## Set the major parameters of the environment being modeled
Sim_horizon =52
Shipping_Delay = 2
Information_Delay = 2
Holding_Cost = 0.5
Backorder_Cost = 1
Initial_Inventory = 12
Integer_Ordering = FALSE

Stable_Start = FALSE

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


## DEFINE SYSTEM REALITY

#'Realty' that the agent is placed into. Note these are read in order.
Sterman89 = TRUE
BaseStock = FALSE
Safety_Stock = NA #If BaseStock = TRUE and Safety_Stock = NA, then safety stock is assumed to be the initial inventory from above
Oliva21 = FALSE
OlivaModel = "Model3" #choices here are Model1 Model2 Model3 Model4
StermanDogan15 = FALSE


## DEFINE AGENT MODELING ASSUMPTIONS

#Agent-Assumed Model of the Environment that the agent is trying to fit to. Note these are read in order.
EstSterman89 = FALSE
EstBaseStock = TRUE
EstSafety_Stock = c(0,24,8,5) #If BaseStock = TRUE and Safety_Stock = NA, then safety stock is assumed to be the initial inventory from above
EstOliva21 = FALSE
EstOlivaModel = "Model3" #choices here are Model1 Model2 Model3 Model4
EstStermanDogan15 = FALSE

## DEFINE AGENT TYPE
BaseStockAgent = TRUE #If not set to True, then assumes a Sterman '89 style model for the agent.
GreedyStepAgent = FALSE #If not set to True, then assumes a Sterman '89 style model for the agent.

##Set the model-based mechanisms in play
#   Forward optimization attempts to reduce costs by optimizing the agent based on 
#     a model of the future that is updated every time step
#   Backward calibration attempts to improve that model by calibrating the parameter
#     estimates of the other entities in the supply chain by matching observed
#     past behavior with the model of the system
#   Note that if the agent is a base-stock ordering agent, then these flags do not matter
Forward_Optimization = TRUE
Backward_Calibration = TRUE

Calibration_Interval = 1

#If set to TRUE, the below flag has the forward optimization use a linear model of the observed customer orders to estiamte future demand
#   when simulating and optimizing for the future. Otherwise, if set to FALSE, the agent is assumed to have full know
#   If the Simple_Forward_Estimate is set to TRUE, then the linear model is static, and either the mean of the demand stream or the most recent observed value
Optimization_Estimate_Linear_Model = TRUE
Simple_Forward_Estimate = TRUE


#Use an Initially optimized or otherwise specified guess for the agent
#Note index 0 is from optimizing each entity separately against an average team
Pre_Optimized_Agent_Parameter_File = "Opt Parameter Table.csv"
Initial_Opt_Index = 0


#Note that these parameters apply to Sterman '89 only. The other two models only have one set of fixed parameters
#Choose how the decision rules for the entities in the supplychain are selected
#Choose team from the input file to simulate against. Note Team 0 is the average performance

#Supply_Chain_Parameter_File = "JS Parameter Table.csv"
#Team_Index_Vector = seq(0,11,by=1)

Supply_Chain_Parameter_File = "Combined Historic Fitted JS Parameter Table.csv"
Team_Index_Vector = seq(0,48,by=1)
#Team_Index_Vector = c(2,3,20,23,34,37,39,43)
#Team_Index_Vector = c(3,20,23,34,37,39,43)


# Note, if both set to 'FALSE' then the values from the team matching the Team_Index is used
Random_Team = FALSE      #If set to TRUE then an entire team will be randomly chosen from the 'JS Parameter Table' file
Random_Entities = FALSE  #If set to TRUE then a purely fictional team will be bootstrapped by combining individuals from the 'JS Parameter Table' file
PreSelectedBootstrapTeams = FALSE
Boostrap_Parameter_File = "Bootsrapped Parameter Table.csv"
NumBootsraps = seq(12,48,by=1)
##Mechanism Horizons
#   The number of time steps that the optimization looks forward when modeling the
#     future and the number of timesteps backward the agent considers when
#     calibrating to history
#   IF SET TO NA then the Optimization Horizon is assumed to be the Simulation Horizon
#     and the Calibration Memory is assumed to be all observed time

Optimization_Horizon = 30#Forward Looking

#Optimization_Horizon = 5

Calibration_Memory = 10 #Backward looking
Optimize_Beyond_Horizon = TRUE  #Set to True to avoid end-game optimization
#Note if Optimization_Horizon is not a number
# then the optimization will only look to the
# end of the simulation horizon.


#Initial Guess of Parameters of the other players:
Initial_Parameter_df_Estimate = data.frame(theta = rep(0.36,4),
                                           alpha_s = rep(0.26,4),
                                           beta = rep(0.34,4),
                                           S_prime = rep(17,4))

##How much information to display while running the script
#   Choose Simple to just get time step updates, Verbose for all updates, 
#   or None for just the final outputs
Output_Comms = "Simple" 

##Optimization Reward Function Shape
Reward = "Cost" #value of 'Cost' or 'Amp'
fnscale = 1 #Set to 1 to minimize the above reward, or set to -1 to maximize the above reward

##Parallalization settings
#   Number of logical cores to 'hold back' from the parallelization. 
#   If set to 0 all logical cores are used
Reserved_CPUs = 1

##Axial search flag
#   Set to TRUE to perform a local axial search after each optimization
#   This is helpful in parallelized environments to improve results at a small
#   time cost.
axial_improvement_search = FALSE

### CALIBRATION WEIGHTS AND OBSERVABLE STATE SPACE

#Set relative weights of each observable part of the system
# Note that the full possible observation space is:
# "Order_flows", "Shipping_flows", "OH_Inventory", "Backorder", "Production_Request"

#Two Calibration types here: 
#   "Full" - Along the full state space 
#   "Observed_Only" - Along only those state or flow values actually observed by the agent

#Assumes the agent can fit to all state variables in the system
CalibrationType = "Full"
Full_CalWeights = c(1,1,1,1,1)
names(Full_CalWeights) = c("Order_flows", "Shipping_flows", "OH_Inventory", "Backorder", "Production_Request")

#Assumes the agent can only see what is 'traditionally' seen in the beer game, including data that is discouraged but still visible
# Orders_Received: Only the orders the agent has personally received
# Shipping_flows: ALL shipping flows amongst all entities (as is visible during the beer game)
# OH_Inventory: ALL on-hand inventory stocks of all entities (as is visible during the beer game)

#CalibrationType = "Observed_Only"
#Observed_CalWeights = c(1,1,1)
#names(Observed_CalWeights) = c("Order_Received", "Shipping_flows", "OH_Inventory")


#Assumes the agent can only see what their own state variables and no others
# Orders_Received: Only the orders the agent has personally received
# Shipping_flows: Only the inbound shipping flows of the entity (but both shipments)
# OH_Inventory: Only the on-hand inventory stocks of the entity alone

#CalibrationType = "Small_Observed_Only"
#Observed_CalWeights = c(1,1,1)
#names(Observed_CalWeights) = c("Order_Received", "Shipping_flows", "OH_Inventory")

####  MODEL-BASED OPTIMIZATION AND LEARNING SETUP ####

#Initialize the history lists
Model_Agent_Performance_List=list()
Default_Agent_Performance_List=list()
No_Agent_Performance_List=list()

#Modify loop depending on which model type was chosen
if (Oliva21 == TRUE || StermanDogan15 == TRUE) {
  Team_Index_Vector = c(1)
}

if (PreSelectedBootstrapTeams == TRUE) {
  if (length(NumBootsraps) == 1) {
    Team_Index_Vector = seq(1,NumBootsraps,by=1)
  } else {
    Team_Index_Vector = NumBootsraps
  }
  
}


#Pre-generate the order strings
ResetHorizon = Sim_horizon + Optimization_Horizon
Global_Orders = setup_orders(OrderType = OrderType, horizon = ResetHorizon, Num_Draws = max(Team_Index_Vector+1), NormalDrawMean= NormalDrawMean, NormalDrawSD=NormalDrawSD)


#START LOOP OVER TEAMS
for (Team_Index in Team_Index_Vector) {
  
  if (Sterman89 == TRUE) {print(paste("Running Experiment on Team Number",Team_Index))}
  if (BaseStock == TRUE) {print(paste("Running Experiment on Base-Stock Team"))}
  if (Oliva21 == TRUE) {print(paste("Running Experiment on Oliva '21 average team based on",OlivaModel))}
  if (StermanDogan15 == TRUE) {print(paste("Running Experiment on Sterman & Dogan '15 average team"))}
  
  #Get the order sting for this team
  Orders = as.vector(Global_Orders[Team_Index + 1,])
  
  #reset parameter estimates
  if (EstBaseStock == TRUE) {
    Parameter_df_Estimate = EstSafety_Stock
  } else {
    Parameter_df_Estimate = Initial_Parameter_df_Estimate
  }

  
  ###Clean up and system Setup
  
  #Record time for start of experiment
  start = Sys.time()
  
  #Import the relevant ordering parameters
  if (Sterman89 == TRUE){
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
    
    
    #Set the actual Ground-Truth of ordering Parameters of the other players:
    True_Parameter_df = as.data.frame(SupplyChainParameters$Par)
    #Save a backup of the full original parameters for later comparison
    Original_True_Parameter_df = True_Parameter_df
  }
  
  if (GreedyStepAgent == TRUE) {
    
    #Set the initial order sequence guess for the greedy step-by-step agent
    Initial_guess = rep(4, Optimization_Horizon)
    
  } else {
    
    #Setup the Initial Guess for cost-reducing Parameters for the Agent using a Base Stock model
    
    if (BaseStockAgent == TRUE) {
      
      Initial_guess = c(0,24,8,5)[AI_Entity_Index]
      
    } else {
      
      #Setup the Initial Guess for cost-reducing Parameters for the Agent using the Sterman 89 model
      
      if (is.numeric(Initial_Opt_Index)){
        #Import the pre-optimized starting Agent Parameters
        AgentPreOptParameters = load_team_parameters(filename = Pre_Optimized_Agent_Parameter_File, 
                                                     Team_Index = Initial_Opt_Index, 
                                                     Random_Team = FALSE, 
                                                     Random_Entities=FALSE)
        Full_Initial_guess = as.matrix(AgentPreOptParameters$Par)
        Initial_guess = Full_Initial_guess[AI_Entity_Index,]
        rownames(Initial_guess)=NULL
        
      } else {
        Initial_guess = c(0.36,0.26,1,36)
        names(Initial_guess) = c("teta","alpha_s","beta","S_prime")
      }
      
    }
    
  }

  #Rewrite the name 'Order_Received' to 'Order_Flows' to match the data format
  # Note that this is skipped if the user specified the weight as 'Order_Flows'
  #   or just didn't include this at all to begin with
  if ((CalibrationType == "Observed_Only") | ((CalibrationType == "Small_Observed_Only"))){
    
    CalWeights = Observed_CalWeights
    CalWeightsNames = names(CalWeights)
    CalWeightsNames[CalWeightsNames == "Order_Received"] = "Order_flows"
    
    names(CalWeights) = CalWeightsNames
    
  } else {
    CalWeights = Full_CalWeights
  }
  
  #Enforce stable start if that flag is present
  if (Stable_Start == TRUE) {
    
    #This ensures the initial inventory for each entity is set such that they are 
    # at a base-stock level at time 0
    # Note that in 'real' runs of the beer game, this is not necessarily true
    Initial_Supply_Line = Shipping_Delay*4
    Initial_Inventory = S_prime - beta*Initial_Supply_Line
    
  }
  
  ### Reset the system to the starting values and update global variables ###
  ResetHorizon = Sim_horizon + Optimization_Horizon
  Reset_List = reset_game(Shipping_Delay = Shipping_Delay, Information_Delay = Information_Delay, Initial_Inventory = Initial_Inventory, 
                          horizon = ResetHorizon
  )
  list2env(Reset_List, envir = .GlobalEnv)
  
  #Parameter Bounds for the optimization and calibration
  # Note that this is over N-1 entities as the calibration has no control over the
  # agents actions
  
  if (EstBaseStock == TRUE) {
    
    CalLowerBounds = rep(0,3)
    CalUpperBounds = rep(Inf,3)
    
  } else {
    
    CalLowerBounds =        data.frame(theta = rep(0,3),
                                       alpha_s = rep(0,3),
                                       beta = rep(0,3),
                                       S_prime = rep(0,3))
    #Initial Guess of Parameters of the other players:
    CalUpperBounds =        data.frame(theta = rep(1,3),
                                       alpha_s = rep(1,3),
                                       beta = rep(1,3),
                                       S_prime = rep(Inf,3))
    
  }
  
  #Initialize Agent Parameter History Dataframe
  if (GreedyStepAgent == TRUE) {
    Agent_Order_History = rep(NA,Sim_horizon+1)
  } else {
    if (BaseStockAgent == TRUE) {
      Agent_Parameter_History = rep(NA,Sim_horizon+1)
    } else {
      Agent_Parameter_History = array(NA,dim=c(4,Sim_horizon+1))
    }
  }
  
  
  #Initialize Parameter Estimate History Dataframe (for later analysis only)
  
  if (EstBaseStock == TRUE) {
    
    Est_Parameter_History = array(NA,dim=c(4,Sim_horizon))
    Est_Parameter_History[,1:(Sim_horizon)] = Parameter_df_Estimate
    
  } else {
    
    Est_Parameter_History = array(NA,dim=c(4,4,Sim_horizon))
    Est_Parameter_History[,,1:(Sim_horizon)]=as.matrix(Parameter_df_Estimate)
    colnames(Est_Parameter_History) = c("theta","alpha_s","beta","S_prime")
    
  }
  

  #Initialize the vectors needed for the optimization and calibration
  if (GreedyStepAgent != TRUE) {
    Agent_Parameter_Guess = Initial_guess

    
    if (BaseStockAgent == TRUE) {
      Agent_Parameter_History[1:length(Agent_Parameter_History)] = unname(Initial_guess)
    }else{
      Agent_Parameter_History[,1:ncol(Agent_Parameter_History)] = unname(Initial_guess)
    }

    #Save the original ordering parameters for later comparison
    Original_Agent_Parameter_History = Agent_Parameter_History

  } else {
    
    Agent_Parameter_Guess = Initial_guess
    Agent_Order_History[1] = 4
    
  }
  
  #Set up the calibration bounds
  CalLower = unlist(CalLowerBounds)
  CalUpper = unlist(CalUpperBounds)
  
  if (GreedyStepAgent != TRUE) {

    
    if (BaseStockAgent == TRUE) {
      
      OptLower = c(0)
      OptUpper = c(Inf)
      
    } else {
    
      OptLower = c(0,0,0,0)
      OptUpper = c(1,1,1,Inf)
      
    }

    
  } else {
    OptLower = rep(0, Optimization_Horizon)
    OptUpper = rep(100, Optimization_Horizon)
  }
  
  
  #Parallelization Setup
  SimPlatformOS = tolower(.Platform$OS.type)
  Global_Var_Export_List = c(names(Reset_List),c("Orders","insertRow","BeerGame_Optimization_with_History","PirateBeerGame_Opt"))
  
  
  ####  MODEL-BASED OPTIMIZATION AND LEARNING MAIN LOOP ####
  
  print(paste("####Beginning Model-Based Simulation####"))
  
  print(paste("Simulating over a horizon of",Sim_horizon,"time steps"))
  
  if (Forward_Optimization != FALSE) {
    print(paste("Optimizing with a forward looking horizon of",Optimization_Horizon,"time steps"))
  }
  if (Backward_Calibration != FALSE) {
    if (is.numeric(Calibration_Memory)){
      print(paste("Calibrating model with a backward looking memory of",Calibration_Memory,"time steps"))
    } else{
      print(paste("Calibrating model with a backward looking memory of all time"))
    }
  }
  
  ##Debug
  time_step = 1
  
  for (time_step in 1:(Sim_horizon)) {
    
    #Parallelization Setup
    if(SimPlatformOS != "windows"){
      cl <- makeCluster(spec=detectCores()-Reserved_CPUs, type="FORK", outfile="")
    } else
      cl <- makeCluster(spec=detectCores()-Reserved_CPUs, outfile="")
    
    setDefaultCluster(cl=cl)
    clusterExport(cl, Global_Var_Export_List)
    
    if (Output_Comms != "None"){
      if (Sterman89 == TRUE) {
        print(paste("Team Index =",Team_Index," and Time step = ",time_step,sep=" "))
      } else {
        print(paste("Time step = ",time_step,sep=" "))
      }
    }
    
    #Calibration objective is to minimize the sum of squared weighted residuals
    
    #Set horizon overwhich to calibrate
    # Assumed to only include observed reality
    
    
    
    if (Backward_Calibration != FALSE) {
      
      if (((time_step %% Calibration_Interval)==0) | time_step == 1) {
      
        if (Output_Comms =="Verbose"){
          print(paste("Calibrating Estimate of other Agents based on Prior Observations..."))
        }
        
        Calibration_Horizon = time_step
        
        if (is.numeric(Calibration_Memory)){
          Calibration_Start = max(1,time_step - Calibration_Memory)
        } else {
          Calibration_Start = 1
        }
        
        #Coerce the parameter data frame into a vector by first removing the values
        # being optimized over by the Agent, if needed, and then flattening
        
        
        if (is.numeric(AI_Entity_Index)){

          if (EstBaseStock == TRUE) {
            if (length(Parameter_df_Estimate) == 4) {
              Trimmed_Parameter_df = Parameter_df_Estimate[-AI_Entity_Index]
            } else {
              Trimmed_Parameter_df = Parameter_df_Estimate
            }
          } else {
            Trimmed_Parameter_df = Parameter_df_Estimate[-AI_Entity_Index,]
          }

        } else {
          Trimmed_Parameter_df = Parameter_df_Estimate
        }
        Cal_Guess = unlist(Trimmed_Parameter_df)
        
        if (GreedyStepAgent == TRUE) {
          Agent_Parameter_History = FALSE
        } else {
          Agent_Order_History = FALSE
        }
        
        Scaling = 1 #Set to 1 for a minimization problem, -1 for a maximization problem
        
        #Reset_List = reset_game(Shipping_Delay = Shipping_Delay, Initial_Inventory = Initial_Inventory, horizon = time_step+1)
        #list2env(Reset_List, envir = .GlobalEnv)
        
        #Calibrate by minimizing the calibration function defined above
        #Note the optimParallel package assumes method = "L-BFGS-B" 
        
        DebugFlag = FALSE
        if (DebugFlag == TRUE) {
          
          if (time_step >=5){
            time_step = time_step
          
          
            Test_Guess = Cal_Guess
            Test_Guess = c(17,17,17)
            
            TestCalOutput =  CalibrationFunction(Parameter_Vector_Estimate = Test_Guess,
                          True_Parameter_df = True_Parameter_df,
                          CalWeights = CalWeights, CalibrationType = CalibrationType,
                          Agent_Parameter_Guess=Agent_Parameter_Guess,
                          Agent_Parameter_History=Agent_Parameter_History,
                          Agent_Order_History = Agent_Order_History,
                          AI_Entity_Index=AI_Entity_Index,
                          Holding_Cost = Holding_Cost,
                          Backorder_Cost = Backorder_Cost,
                          Orders = Orders,
                          Sterman89 = Sterman89, Oliva21 = Oliva21, OlivaModel = OlivaModel, StermanDogan15 = StermanDogan15,
                          BaseStock = BaseStock, Safety_Stock = Safety_Stock,
                          BaseStockAgent = BaseStockAgent,
                          GreedyStepAgent = GreedyStepAgent,
                          EstSterman89 = EstSterman89, EstOliva21 = EstOliva21, EstOlivaModel = EstOlivaModel, EstStermanDogan15 = EstStermanDogan15,
                          EstBaseStock = EstBaseStock, EstSafety_Stock = EstSafety_Stock,
                          Integer_Ordering = Integer_Ordering,
                          Calibration_Start=Calibration_Start,
                          Calibration_Horizon=time_step
                          
            )
          }
            
        }
        
        
        
        Cal_output = optimParallel(par = Cal_Guess, fn = CalibrationFunction, control=list(fnscale=1), parallel=list(forward=FALSE),
                                   True_Parameter_df = True_Parameter_df,
                                   CalWeights = CalWeights, CalibrationType = CalibrationType,
                                   Agent_Parameter_Guess=Agent_Parameter_Guess,
                                   Agent_Parameter_History=Agent_Parameter_History,
                                   Agent_Order_History = Agent_Order_History,
                                   AI_Entity_Index=AI_Entity_Index,
                                   Holding_Cost = Holding_Cost,
                                   Backorder_Cost = Backorder_Cost,
                                   Orders = Orders,
                                   Sterman89 = Sterman89, Oliva21 = Oliva21, OlivaModel = OlivaModel, StermanDogan15 = StermanDogan15,
                                   BaseStock = BaseStock, Safety_Stock = Safety_Stock,
                                   BaseStockAgent = BaseStockAgent,
                                   GreedyStepAgent = GreedyStepAgent,
                                   EstSterman89 = EstSterman89, EstOliva21 = EstOliva21, EstOlivaModel = EstOlivaModel, EstStermanDogan15 = EstStermanDogan15,
                                   EstBaseStock = EstBaseStock, EstSafety_Stock = EstSafety_Stock,
                                   Integer_Ordering = Integer_Ordering,
                                   Calibration_Start=Calibration_Start,
                                   Calibration_Horizon=time_step,
                                   lower = CalLower , upper = CalUpper
                                   
        )
        
        #Extract the results from the calibration

        if (EstBaseStock == TRUE) {
          Cal_Opt_par_No_Agent = unname(Cal_output$par)
        }else{
          Cal_Opt_par_No_Agent = as.data.frame(array(unname(Cal_output$par),dim=c(3,4)))
        }
 
        if (GreedyStepAgent != TRUE) {

          if (EstBaseStock == TRUE) {
            Cal_Opt_par = append(Cal_Opt_par_No_Agent,Agent_Parameter_History[time_step],after = (AI_Entity_Index-1))
          }else{
            Cal_Opt_par = insertRow(Cal_Opt_par_No_Agent,Agent_Parameter_History[,time_step],AI_Entity_Index)
            names(Cal_Opt_par) = c("theta","alpha_s","beta","S_prime")
          }

        } else {
          Cal_Opt_par = insertRow(Cal_Opt_par_No_Agent,rep(NA,4),AI_Entity_Index)
          names(Cal_Opt_par) = c("theta","alpha_s","beta","S_prime")
        }

        
        
        #Check to see if the calibration has changed after initial round

        if (EstBaseStock == TRUE) {
          ParameterCheck = (Parameter_df_Estimate[-AI_Entity_Index] == Cal_Opt_par_No_Agent)
        } else {
          ParameterCheck = (Parameter_df_Estimate[-AI_Entity_Index,] == Cal_Opt_par_No_Agent)
        }

        if ((sum(ParameterCheck) == length(ParameterCheck)) & (time_step>1)) {
          
          Skip_Optimization_Flag = TRUE
          
        } else {
          
          Skip_Optimization_Flag = FALSE
          
        }
        
        #Store the results as the new estimate of the other entities ordering parameters
        Parameter_df_Estimate = Cal_Opt_par
        
        ###NEW###
        #Maybe not needed
        if (EstBaseStock == TRUE) {
          EstSafety_Stock = Parameter_df_Estimate
        }
        ###/NEW###
        
        if (Output_Comms =="Verbose"){
          print("Estimated parameters from other players:")
          print(Cal_Opt_par)
        }
      } else {
        Skip_Optimization_Flag = TRUE ###!!DOUBLE CHECK THIS
      }
    } #End Calibration
    
    if ((Forward_Optimization != FALSE) & (Skip_Optimization_Flag == FALSE)) {
      
      #Optimize based on the estimated ordering parameters of the other agents and the agent's own history
      if (Output_Comms =="Verbose"){
        print(paste("Optimizing Agent Based on Future Expected Performance..."))
      }
      
      #Determine the end time of the optimization
      if (is.numeric(Optimization_Horizon)){
        if (Optimize_Beyond_Horizon == TRUE){
          Optimization_End_Time = Optimization_Horizon+time_step
        } else {
          Optimization_End_Time = min(Sim_horizon,Optimization_Horizon+time_step)
        }
      } else {
        Optimization_End_Time = Sim_horizon
      }
      
      if (Optimization_Estimate_Linear_Model == TRUE) {
        
        #Updated estimate of underlying order pattern based on observed history
        if (is.numeric(Calibration_Memory)){
          ObsOrders = Orders[max(1,time_step-Calibration_Memory):max(1,time_step-1)]
          ObsTime = seq(from = max(1,time_step-Calibration_Memory), to = max(1,time_step-1), by = 1)
        } else {
          ObsOrders = Orders[1:max(1,time_step-1)]
          ObsTime = seq(from = 1, to = max(1,time_step-1), by = 1)
        }
        
        if (Simple_Forward_Estimate == TRUE) {
          
          if (OrderType == "S89") {
            
            #For sterman '89 orders, the agent assumes observed order will continue until they see otherwise
            
            PredOrders=rep(tail(ObsOrders, n = 1), ResetHorizon+1 - time_step + 1)
            
          }
          
          if (OrderType == "CD06") {
            
            #For CD06 agents, they assume that future orders will be average
            EstAvgOrder = (UniformDrawMin + UniformDrawMax)/2  
            PredOrders=rep(EstAvgOrder, ResetHorizon+1 - time_step + 1)
            
          }
          
          if (OrderType == "CS00") {
            
            # for CS00 agents they assume that future orders will be average
            PredOrders=rep(NormalDrawMean, ResetHorizon+1 - time_step + 1)
            
          }
          
          ObsAndPredOrders = c(Orders[1:(time_step-1)],PredOrders)
          
        } else {
          
          ObsData = data.frame(x = ObsTime,
                               y = ObsOrders)
          EstOrderModel = lm(y ~ x, data = ObsData)
          
          PredTime = seq(from = time_step, to = ResetHorizon+1, by = 1)
          
          PredData = data.frame(x = PredTime)
          
          PredOrders = as.vector(predict(EstOrderModel,newdata = PredData))
          
          ObsAndPredOrders = c(Orders[1:(time_step-1)],PredOrders)
          
        }  
      } else {
        ObsAndPredOrders = Orders
      }
      
      #Reset global arrays to match the current size of the optimization
      #Reset_List = reset_game(Shipping_Delay = Shipping_Delay, Initial_Inventory = Initial_Inventory, horizon = Optimization_End_Time)
      #list2env(Reset_List, envir = .GlobalEnv)
      
      #Note the optimization assumes the Sterman '89 model looking forward here
      Opt_Initial_Guess = unname(Agent_Parameter_Guess)
      
      if (BaseStockAgent == TRUE) {
        Opt_Initial_Guess = unlist(unname(Parameter_df_Estimate[AI_Entity_Index]))
      }else {
        Opt_Initial_Guess = unlist(unname(Parameter_df_Estimate[AI_Entity_Index,]))
      }

      Opt_output = optimParallel(par = Opt_Initial_Guess, fn = BeerGame_Optimization_with_History, control=list(fnscale=fnscale), 
                                 parallel = list(forward=FALSE),
                                 Agent_Parameter_History = Agent_Parameter_History,
                                 Agent_Order_History = Agent_Order_History,
                                 AI_Entity_Index = AI_Entity_Index,
                                 BaseStockAgent = BaseStockAgent,
                                 GreedyStepAgent = GreedyStepAgent,
                                 Integer_Ordering = Integer_Ordering,
                                 Sterman89 = EstSterman89, Oliva21 = EstOliva21, OlivaModel = EstOlivaModel, 
                                 StermanDogan15 = EstStermanDogan15, #Whatever model when optimizing
                                 BaseStock = EstBaseStock, Safety_Stock = EstSafety_Stock, 
                                 start_time = time_step-1, 
                                 horizon = Optimization_End_Time,
                                 Holding_Cost = Holding_Cost,
                                 Backorder_Cost = Backorder_Cost,
                                 Orders = ObsAndPredOrders, #Assumes that the order signal is the observed plus the forward estiamted
                                 Reward = Reward, 
                                 Parameter_df = Parameter_df_Estimate, #Assumes estimated parameters when estimating
                                 Myopic_Reward = Myopic_Reward,
                                 Selfweight = Selfweight,
                                 lower = OptLower , upper = OptUpper
      )
      
      #Update the agent's order behavior based on the output from the optimization
      Opt_par = unname(Opt_output$par)
      
      if (axial_improvement_search==TRUE){
        if (Output_Comms =="Verbose"){
          print("Axial Searching for Improvements")
        }
        axial_search = axsearch(par = unname(Opt_par), fn = BeerGame_Optimization_with_History, fmin = Opt_output$value,
                                Agent_Parameter_History = Agent_Parameter_History,
                                Agent_Order_History = Agent_Order_History,
                                AI_Entity_Index = AI_Entity_Index,
                                BaseStockAgent = BaseStockAgent,
                                GreedyStepAgent = GreedyStepAgent,
                                Integer_Ordering = Integer_Ordering,
                                Sterman89 = TRUE, Oliva21 = FALSE, StermanDogan15 = FALSE,
                                BaseStock = FALSE, Safety_Stock = NA,
                                start_time = time_step-1, 
                                horizon = Optimization_End_Time,
                                Holding_Cost = Holding_Cost,
                                Backorder_Cost = Backorder_Cost,
                                Orders = ObsAndPredOrders,
                                Reward = Reward, 
                                Parameter_df = Parameter_df_Estimate,
                                Myopic_Reward = Myopic_Reward,
                                Selfweight = Selfweight,
                                lower = OptLower , upper = OptUpper)
        
        if (axial_search$bestfn<Opt_output$value) {
          if (Output_Comms =="Verbose"){
            improvement = percent((axial_search$bestfn-Opt_output$value)/Opt_output$value,digits=8)
            print(paste("Improvement of ",improvement,"... Saving improved values..."),sep="")
          }
          Opt_par = unname(axial_search$par)
        } else {
          if (Output_Comms =="Verbose"){
            print("No improvement found.")
          }
        }
      }
      
      if (GreedyStepAgent != TRUE) {

        if (BaseStockAgent == TRUE) {
          
          Agent_Parameter_Guess = Opt_par
          Agent_Parameter_History[time_step+1] = Opt_par
          Parameter_df_Estimate[AI_Entity_Index] = Agent_Parameter_Guess
          
        }else{
        
          names(Opt_par) = c("teta","alpha_s","beta","S_prime")
          Agent_Parameter_Guess = Opt_par
          Agent_Parameter_History[,time_step+1] = unname(Opt_par)
          
          Parameter_df_Estimate[AI_Entity_Index,] = Agent_Parameter_Guess
        
        }

      } else {
        
        Agent_Parameter_Guess = Opt_par
        Agent_Order_History[time_step+1] = Opt_par[1] #commit to the next action
        
      }
      
      #Store estimated parameters for later analysis.
      # Note that this object includes the optimized agent parameters, which
      # should be ignored or removed prior to any analysis of error or convergence

      if (EstBaseStock == TRUE) {
        Est_Parameter_History[,time_step] = Parameter_df_Estimate
      }  else {
        Est_Parameter_History[,,time_step] = as.matrix(Parameter_df_Estimate)
      } 

      if (Output_Comms =="Verbose"){
        print("Current Agent Ordering Parameters:")
        print(Agent_Parameter_Guess)
      }
      
      
    } else {
      #Still record the agent parameter action even if no optimization occurred this round
      
      if (BaseStockAgent == TRUE) {
        Agent_Parameter_Guess = Parameter_df_Estimate[AI_Entity_Index]
        Agent_Parameter_History[time_step+1] = Agent_Parameter_Guess
        Parameter_df_Estimate[AI_Entity_Index] = Agent_Parameter_Guess
        
      } else {
        Agent_Parameter_Guess = Parameter_df_Estimate[AI_Entity_Index,]
        Agent_Parameter_History[,time_step+1] = unlist(unname(Agent_Parameter_Guess))
        
        Parameter_df_Estimate[AI_Entity_Index,] = Agent_Parameter_Guess
      }

    }
    
    #Cleanup parallelization
    setDefaultCluster(cl=NULL)
    if (time_step<Sim_horizon){
      stopCluster(cl)
    }
    
  } #next time step
  
  #Record time it took to do the above
  end = Sys.time()
  time_to_optimize = difftime(end,start,units="secs")
  
  #Cleanup parallelization
  setDefaultCluster(cl=NULL)
  stopCluster(cl)
  
  print(paste("Experiment Ended in",round(time_to_optimize,2),"seconds. Saving Results..."))
  
  ####  TALLY AGENT PERFORMANCE AND SIMULATE COUNTER-FACTUALS ####
  
  if (Sterman89 == TRUE) {
    OutputParameter_df = True_Parameter_df
  } else {
    OutputParameter_df = FALSE
  }
  
  #Reset global arrays and simulate costs incurred by the system with the agent 
  #   in place given its ordering history
  Reset_List = reset_game(Shipping_Delay = Shipping_Delay, Information_Delay = Information_Delay, 
                          Initial_Inventory = Initial_Inventory, horizon = Sim_horizon
  )
  list2env(Reset_List, envir = .GlobalEnv)
  
  if ((GreedyStepAgent == TRUE) | (BaseStockAgent == TRUE)) {
    OutputAgent_parameters = FALSE
  }else {
    OutputAgent_parameters = Agent_Parameter_History[,Sim_horizon]
  }
  
  Model_Agent_Performance = BeerGame_Optimization_with_History(Agent_Parameter = OutputAgent_parameters,
                                                               Agent_Parameter_History= Agent_Parameter_History,
                                                               Agent_Order_History = Agent_Order_History,
                                                               AI_Entity_Index = AI_Entity_Index,
                                                               BaseStockAgent = BaseStockAgent,
                                                               GreedyStepAgent = GreedyStepAgent,
                                                               Integer_Ordering = Integer_Ordering,
                                                               Sterman89 = Sterman89, Oliva21 = Oliva21, OlivaModel = OlivaModel, StermanDogan15 = StermanDogan15,
                                                               BaseStock = BaseStock, Safety_Stock = Safety_Stock,
                                                               start_time = Sim_horizon, horizon = Sim_horizon, 
                                                               Parameter_df = OutputParameter_df,
                                                               Holding_Cost = Holding_Cost, 
                                                               Backorder_Cost = Backorder_Cost, 
                                                               Orders = Orders,
                                                               Reward = "Cost", 
                                                               Mode = "Calibration")
  Final_Orders = unname(rbind(Model_Agent_Performance$Order_flows[2:4,Information_Delay,],Model_Agent_Performance$Production_Request))
  Model_Agent_Performance$Final_Orders = Final_Orders
  Model_Agent_Cost = unlist(Model_Agent_Performance$Reward)
  
  #Reset global arrays and simulate counter factual costs incurred by the system 
  #   with the initial fixed agent in place
  Reset_List = reset_game(Shipping_Delay = Shipping_Delay, Information_Delay = Information_Delay, 
                          Initial_Inventory = Initial_Inventory, horizon = Sim_horizon
  )
  list2env(Reset_List, envir = .GlobalEnv)
  

  if (BaseStockAgent == TRUE) {
    Static_Agent_Parameters = Original_Agent_Parameter_History[Sim_horizon]
  } else {
    Static_Agent_Parameters = Original_Agent_Parameter_History[,Sim_horizon]
  }
  
  Default_Agent_Performance = BeerGame_Optimization_with_History(Agent_Parameter = Static_Agent_Parameters,
                                                                 Agent_Parameter_History= Original_Agent_Parameter_History,
                                                                 Agent_Order_History = Agent_Order_History,
                                                                 AI_Entity_Index = AI_Entity_Index,
                                                                 BaseStockAgent = BaseStockAgent,
                                                                 GreedyStepAgent = FALSE,
                                                                 Integer_Ordering = Integer_Ordering,
                                                                 Sterman89 = Sterman89, Oliva21 = Oliva21, OlivaModel = OlivaModel, StermanDogan15 = StermanDogan15,
                                                                 BaseStock = BaseStock, Safety_Stock = Safety_Stock,
                                                                 start_time = Sim_horizon, horizon = Sim_horizon, 
                                                                 Parameter_df = OutputParameter_df,
                                                                 Holding_Cost = Holding_Cost, 
                                                                 Backorder_Cost = Backorder_Cost, 
                                                                 Orders = Orders,
                                                                 Reward = "Cost", 
                                                                 Mode = "Calibration")

  Final_Orders = unname(rbind(Default_Agent_Performance$Order_flows[2:4,Information_Delay,],Default_Agent_Performance$Production_Request))
  Default_Agent_Performance$Final_Orders = Final_Orders
  Default_Agent_Cost = unlist(Default_Agent_Performance$Reward)
  
  #Reset global arrays and simulate counter factual costs incurred by the system 
  #   with no agent at all in place
  Reset_List = reset_game(Shipping_Delay = Shipping_Delay, Information_Delay = Information_Delay, 
                          Initial_Inventory = Initial_Inventory, horizon = Sim_horizon
  )
  list2env(Reset_List, envir = .GlobalEnv)
  No_Agent_Performance = BeerGame_Optimization_with_History(Agent_Parameter = FALSE,
                                                            Agent_Parameter_History= FALSE,
                                                            Agent_Order_History = FALSE,
                                                            AI_Entity_Index = FALSE,
                                                            BaseStockAgent = FALSE,
                                                            GreedyStepAgent = FALSE,
                                                            Integer_Ordering = Integer_Ordering,
                                                            Sterman89 = Sterman89, Oliva21 = Oliva21, OlivaModel = OlivaModel, StermanDogan15 = StermanDogan15,
                                                            BaseStock = BaseStock, Safety_Stock = Safety_Stock,
                                                            start_time = Sim_horizon, horizon = Sim_horizon, 
                                                            Parameter_df = OutputParameter_df,
                                                            Holding_Cost = Holding_Cost, 
                                                            Backorder_Cost = Backorder_Cost, 
                                                            Orders = Orders,
                                                            Reward = "Cost", 
                                                            Mode = "Calibration")
  Final_Orders = unname(rbind(No_Agent_Performance$Order_flows[2:4,Information_Delay,],No_Agent_Performance$Production_Request))
  No_Agent_Performance$Final_Orders = Final_Orders
  No_Agent_Cost = unlist(No_Agent_Performance$Reward)
  
  
  
  Defualt_Agent_Improvement = (Default_Agent_Cost - No_Agent_Cost)/No_Agent_Cost
  Model_Agent_Improvement = (Model_Agent_Cost - No_Agent_Cost)/No_Agent_Cost
  
  Relative_Model_Advantage = Model_Agent_Improvement - Defualt_Agent_Improvement
  
  
  ####  OUTPUTS AND REPORTS ####
  
  if (BaseStockAgent == TRUE) {
    OUTPUT_Agent_Parameter_History = Agent_Parameter_History[1:Sim_horizon]
  }else{
    OUTPUT_Agent_Parameter_History = Agent_Parameter_History[,1:Sim_horizon]
    rownames(OUTPUT_Agent_Parameter_History) = c("theta","alpha_s","beta","S_prime")
  }
  
  if (EstBaseStock == TRUE) {
    OUTPUT_LEARNED_PARAMETERS = Est_Parameter_History[,1:Sim_horizon]
    OUTPUT_LEARNED_PARAMETERS[AI_Entity_Index,] = NA
  }else {
    OUTPUT_LEARNED_PARAMETERS = Est_Parameter_History[,,1:Sim_horizon]
    OUTPUT_LEARNED_PARAMETERS[AI_Entity_Index,,] = NA
  }
  
  if ((EstSterman89 == TRUE) & (Sterman89 == TRUE)) {
  
    #Take Scaled Frobenius Norm of the learned parameters and the ground-truth parameters
      
    #Scale the parameter matricies (since S_prime can be 100x larger than the other values)
    NormScaling = c(1,1,1,100)
    names(NormScaling) = c("theta","alpha_s","beta","S_prime")
    
    RowDivide <- function(matrix,vector) {
      t(t(matrix) / vector)
    }
    
    Scaled_True_Parameters = RowDivide(True_Parameter_df,NormScaling)
    
    Scaled_Learned_Parameters = OUTPUT_LEARNED_PARAMETERS
    for (i in 1:Sim_horizon){
      Scaled_Learned_Parameters[,,i] = RowDivide(OUTPUT_LEARNED_PARAMETERS[,,i],NormScaling)
    }
    
    SumAxis = 2
    fnorm_learned = sqrt(colSums((abs(Scaled_Learned_Parameters)^2),na.rm= TRUE, dims=SumAxis))
    fnorm_truth = sqrt(sum((abs(Scaled_True_Parameters[-AI_Entity_Index,])^2),na.rm= TRUE))
    
    Learning_Err = abs((fnorm_learned - fnorm_truth))/fnorm_truth
    
    Model_Agent_Performance$Learning_Err = Learning_Err
  } else {
    Model_Agent_Performance$Learning_Err = NA
  }

  #Save outputs for later analysis
  
  Model_Agent_Performance_List[[as.character(Team_Index)]] = Model_Agent_Performance
  Default_Agent_Performance_List[[as.character(Team_Index)]] = Default_Agent_Performance
  No_Agent_Performance_List[[as.character(Team_Index)]] = No_Agent_Performance
  
  ##Combo Objects
  
  Combo_Learning_Error = array(NA,dim=c(length(Model_Agent_Performance_List),Sim_horizon))
  rownames(Combo_Learning_Error) = names(Model_Agent_Performance_List)
  
  Combo_Reward_Array = array(NA,dim=c(length(Model_Agent_Performance_List),3))
  rownames(Combo_Reward_Array) = names(Model_Agent_Performance_List)
  colnames(Combo_Reward_Array) = c("Baseline","Default Agent","Model-Based Agent")
  
  for (i in 1:length(Model_Agent_Performance_List)) {
    
    Combo_Learning_Error[i,]=Model_Agent_Performance_List[[i]]$Learning_Err
    
    Combo_Reward_Array[i,1] = No_Agent_Performance_List[[i]]$Reward
    Combo_Reward_Array[i,2] = Default_Agent_Performance_List[[i]]$Reward
    Combo_Reward_Array[i,3] = Model_Agent_Performance_List[[i]]$Reward
    
  }
  write.csv(Combo_Reward_Array,"BackupTemp.csv")
  
  
} #Next Team Index


##Combo Objects

Combo_Learning_Error = array(NA,dim=c(length(Model_Agent_Performance_List),Sim_horizon))
rownames(Combo_Learning_Error) = names(Model_Agent_Performance_List)

Combo_Reward_Array = array(NA,dim=c(length(Model_Agent_Performance_List),3))
rownames(Combo_Reward_Array) = names(Model_Agent_Performance_List)
colnames(Combo_Reward_Array) = c("Baseline","Default Agent","Model-Based Agent")

for (i in 1:length(Model_Agent_Performance_List)) {
  
  Combo_Learning_Error[i,]=Model_Agent_Performance_List[[i]]$Learning_Err
  
  Combo_Reward_Array[i,1] = No_Agent_Performance_List[[i]]$Reward
  Combo_Reward_Array[i,2] = Default_Agent_Performance_List[[i]]$Reward
  Combo_Reward_Array[i,3] = Model_Agent_Performance_List[[i]]$Reward
  
}



#### PLOTS ####

legendtext = c("1: Retailer","2: Wholesaler","3: Distributor","4: Factory")

TeamIndexText = names(Model_Agent_Performance_List)

#Learning Rates

#Clear existing plot windows
if(!is.null(dev.list())) graphics.off()

#Set up so plots are in an external window
options(device = "windows")

SavePlots = FALSE

PngFileName = paste("LearningRate-Agent",AI_Entity_Index,".png",sep="")

# Create new output window
dev.new(width=6, height=3.75) 
# We suggest a 6" by 3.75" window to facilitate importing into word document (default values ~golden ratio)

# Adjust the margin of plot (bottom,left,top,right)
par(mar=c(3, 5, 2, 1))
AxisTitleSize = 1

if(SavePlots == TRUE) png(PngFileName, width=6, height=3.75, units="in", res=1200)

TeamName = unlist(SupplyChainParameters$TeamName)

matplot(t(Combo_Learning_Error[,]), type = 'l', main = paste("Learning Rates with Agent at Position",AI_Entity_Index),lwd = 3, lty = 1,
        yaxt = "n",ylab = "",xlab="")
axis(2, at=pretty(t(Combo_Learning_Error[,])), lab=paste0(pretty(t(Combo_Learning_Error[,])) * 100,"%"), las=TRUE)

#Y Label
mtext(expression(bold("Absolute Scaled")),side=2,line=4,cex=AxisTitleSize) 
mtext(expression(bold("FNorm Percent Error")),side=2,line=3,cex=AxisTitleSize)
#X Label
mtext(expression(bold("Time")),side=1,line=2,cex=AxisTitleSize) 

legend("topright",title = "Sterman '89 Team Number",ncol = 2,TeamIndexText, col=seq_len(length(TeamIndexText)), cex=0.8, fill=seq_len(length(TeamIndexText)))





PlotType = "ModelAgent"
PlotType = "Baseline" 
PlotType = "DefaultAgent"

#Sterman89=TRUE

#TeamToPlot =0

#Oliva21=FALSE

if (PlotType == "ModelAgent"){
  PlotObject_List = Model_Agent_Performance_List
  TitleSubText = paste("Model-Aware Agent at Position",AI_Entity_Index)
}
if (PlotType == "Baseline"){
  PlotObject_List = No_Agent_Performance_List
  TitleSubText = paste("Baseline - No Agent Present")
}
if (PlotType == "DefaultAgent"){
  PlotObject_List = Default_Agent_Performance_List
  TitleSubText = paste("Single-Shot Optimized Agent at Position",AI_Entity_Index)
}


if (Sterman89 == TRUE){
  TitleText = paste("Orders Placed - Sterman '89 Environment Team",as.character(TeamToPlot))
  InventoryTitleText = paste("Net Inventory - Sterman '89 Environment Team",as.character(TeamToPlot))
}

if (BaseStock == TRUE){
  TitleText = paste("Orders Placed - Base-Stock Environment")
  InventoryTitleText = paste("Net Inventory - Base-Stock Environment")
  TeamToPlot=1
}

if (Oliva21 == TRUE){
  TeamToPlot = 1
  TitleText = paste("Orders Placed - Oliva '21 Environment")
  InventoryTitleText = paste("Orders Placed - Oliva '21 Environment")
  TeamToPlot=1
}


PlotObject = PlotObject_List[[as.character(TeamToPlot)]]

#ORDERS PLACED PER ROUND#

#Set up so plots are in an external window
options(device = "windows")

## ORDERS PLOT

# Create new output window
dev.new(width=6, height=3.75) 

# Adjust the margin of plot (bottom,left,top,right)
par(mar=c(3, 5, 3, 1))
AxisTitleSize = 1

ymax = round(max(c(PlotObject$Final_Orders,Orders[1:Sim_horizon]))*1.1,0)
#ymax = 35
matplot(t(PlotObject$Final_Orders[,]), type = 'l', main = "", lwd = 3, lty = 1,
        ylim = c(0,ymax),
        ylab = "", xlab = ""
)
lines(Orders, type='l', lty = 4, col = 8, lwd = 3)

#Y Label
mtext(expression(bold("Units Ordered Each Round")),side=2,line=3,cex=AxisTitleSize) 
#X Label
mtext(expression(bold("Time")),side=1,line=2,cex=AxisTitleSize) 

#Two line title
mtext(TitleText,side=3,line=2,cex=1,font=2)
mtext(TitleSubText,side=3,line=1,cex=.9,font=4)
#mtext(expression(bold(paste(TitleText))),side=3,line=1,cex=AxisTitleSize*1.1)

legend("topleft",c("Customer",legendtext), col=c(8,seq_len(4)), cex=0.8, fill=c(8,seq_len(4)))


## INVENTORY PLOT

# Create new output window
dev.new(width=6, height=3.75) 

# Adjust the margin of plot (bottom,left,top,right)
par(mar=c(3, 5, 3, 1))
AxisTitleSize = 1

Net_Inventory = PlotObject$OH_Inventory - PlotObject$Backorder
VerifyCosts = sum(Holding_Cost*PlotObject$OH_Inventory + Backorder_Cost*PlotObject$Backorder,na.rm=TRUE)

ymax = round(max(Net_Inventory)*1.1,0)
ymin = round(min(Net_Inventory)*1.1,0)
#ymax = 35
matplot(t(Net_Inventory[,]), type = 'l', main = "", lwd = 3, lty = 1,
        ylim = c(ymin,ymax),
        ylab = "", xlab = ""
)
#Y Label
mtext(expression(bold("Net Inventory (OH - Backlog)")),side=2,line=3,cex=AxisTitleSize) 
#X Label
mtext(expression(bold("Time")),side=1,line=2,cex=AxisTitleSize) 

#Two line title
mtext(InventoryTitleText,side=3,line=2,cex=1,font=2)
mtext(TitleSubText,side=3,line=1,cex=.9,font=4)
#mtext(expression(bold(paste(TitleText))),side=3,line=1,cex=AxisTitleSize*1.1)

legend("topleft",c("Customer",legendtext), col=c(8,seq_len(4)), cex=0.8, fill=c(8,seq_len(4)))



#Clear all the plot windows after saving
if(SavePlots==TRUE) {
  if(!is.null(dev.list())) graphics.off()
}




write.excel(Combo_Reward_Array)


Combo_Reward_Array

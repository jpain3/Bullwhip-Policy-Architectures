#Function to run the Beer Game for a single time step

PirateBeerGame_Opt <- function(AI_Entity = FALSE, AI_Entity_Index=FALSE, BaseStockAgent = FALSE, AI_parameter_df = FALSE, AI_Order = FALSE, 
                               Sterman89 = TRUE, Oliva21 = FALSE, OlivaModel = "Model3", StermanDogan15 = FALSE, 
                               BaseStock = FALSE, Safety_Stock = NA, 
                               t, Orders, 
                               Order_flows, Shipping_flows, OH_Inventory, Backorder,
                               L, L_hat, Production_Request,
                               Parameter_df = FALSE, Integer_Ordering = FALSE, Noisey_Ordering = FALSE,
                               Shipping_Delay = 2, Information_Delay = 2) {
  
  Backlog_in_Inventory = TRUE
  
  
  
  if (length(Parameter_df) == 1) {
    
    theta = rep(0.36,4)
    alpha_s = rep(0.26,4)
    beta = rep(0.34,4)
    S_prime = rep(17,4)
    
    TeamName = "Default Average Agents"
    
    
  } else {
    
    
    #Repair blanks or zero values
    Parameter_df[is.na(Parameter_df)] = 0
    
    
    #Assign parameter values
    theta = unlist(Parameter_df$theta)
    alpha_s = unlist(Parameter_df$alpha_s)
    beta = unlist(Parameter_df$beta)
    S_prime = unlist(Parameter_df$S_prime)
    
    #TeamName = as.character(Parameter_sub$Team_Name[1])
    
  }
  
  
  if (is.numeric(unname(AI_parameter_df[1]))) {
    
    theta[AI_Entity_Index] = unname(AI_parameter_df["theta"])
    alpha_s[AI_Entity_Index] = unname(AI_parameter_df["alpha_s"])
    beta[AI_Entity_Index] = unname(AI_parameter_df["beta"])
    S_prime[AI_Entity_Index] = unname(AI_parameter_df["S_prime"])
    
  }
  
  if (StermanDogan15 == TRUE) {
    
    psi = 0.61
    theta = 0.38
    alpha_s = 0.33
    beta = 0.22
    gamma = 3.34
    kappa = 21.98
    omega = 2.53
    gamma_max = 621
    
    Order_Type = "Classic"
    
    Normal_Delay = Shipping_Delay + Information_Delay
    
    if (Order_Type == "Stationary"){
      D_c =  Orders[1]
    }
    
    TeamName = paste("Sterman and Dogan '15")
    
  }
  
  if (Oliva21 == TRUE) {
    
    if (OlivaModel == "Model1") {
      beta_L  = 1
      beta_S  = -0.11
      beta_SL = -0.01
      gamma_0   = 0.34
      
    }
    if (OlivaModel == "Model2") {
      
      beta_L  = 1.01
      beta_S  = -0.11
      beta_SL = -0.01
      gamma_0   = 0.28
      
    }
    if (OlivaModel == "Model3") {
      
      beta_L  = 1
      beta_S  = -0.21
      beta_SL = 0.00
      beta_B  = 0.22
      gamma_0   = 1.59
      
    }
    if (OlivaModel == "Model4") {
      
      beta_L  = 1.04
      beta_S  = -0.21
      beta_SL = 0.01
      beta_B  = 0.22
      gamma_0   = 1.22
      
    }
    
    TeamName = paste("Oliva et.al. '21",OlivaModel)
    
  }
  
  #Set up the Safety Stock values
  if (BaseStock == TRUE || BaseStockAgent == TRUE) {
    
    if (is.numeric(Safety_Stock[1])) {
      
      if (length(Safety_Stock) == 1) {
        Safety_Stock = rep(Safety_Stock,4)
      } else {
        Safety_Stock = Safety_Stock
      }
      
    } else {
      Safety_Stock = OH_Inventory[,1]
    }
    
  }
  
  #####Receive Inventory and Advance Shipping Delays#####
  
  #Receive shipments
  
  if (t!= 1) {
    
    OH_Inventory[,t] = OH_Inventory[,t-1] + Shipping_flows[,1,t-1]
    
    #For Sterman and Dogan '15 model
    if (StermanDogan15==TRUE){
      ShipmentsReceived = Shipping_flows[,1,t-1]
    }
    
    #Advance shipping delays
    Shipping_flows[,1:(Shipping_Delay-1),t] = Shipping_flows[,2:(Shipping_Delay),t-1]
    
  }
  
  #####Fill Orders######
  
  #View Orders
  Order_Received = Order_flows[,1,t-1]
  #Add backlog to order pressure (remove to smooth)
  Incoming_Order = Order_Received + Backorder[,t-1]
  
  #Ship what you can
  #PROBLEM: NEED TO ACCOUNT FOR DEMAND FROM BACK ORDER
  Outbound_shipments = pmax(0,pmin(OH_Inventory[,t],Incoming_Order))
  
  #Put shipments into lefthand shipping slot  
  Shipping_flows[1:3,Shipping_Delay,t] = Outbound_shipments[2:4]
  #Shipping_flows[1:3,2,t] = Outbound_shipments[2:4]
  
  #Send shipments from retailer to the customer
  Final_Customer_Orders_Filled[t] = Outbound_shipments[1]
  
  #Determine backlog, if any
  Inventory_Shortage = Order_flows[,1,t-1] - OH_Inventory[,t]
  Backorder[,t] = Backorder[,t-1] + Inventory_Shortage
  Backorder[,t][Backorder[,t]<0] = 0
  
  #Subtract out the order from inventory
  OH_Inventory[,t] = OH_Inventory[,t] - Outbound_shipments
  
  
  #####Advance Order Slips and Brewers Brew######
  
  Order_flows[,1:(Information_Delay-1),t] = Order_flows[,2:Information_Delay,t-1]
  #Order_flows[,1,t] = Order_flows[,2,t-1]
  Order_flows[1,1,t] = Orders[t]
  
  
  #Brewers Brew
  Shipping_flows[4,Shipping_Delay,t] = Production_Request[t-1]
  #Shipping_flows[4,2,t] = Production_Request[t-1]
  
  #####PLACE ORDERS######
  
  
  for (i in 1:4) {
    
    Entity_Index = i
    
    #Observe supply line
    SL = sum(Shipping_flows[Entity_Index,,t]) #Total supply line inbound
    
    #L[Entity_Index,t] = Incoming_Order[Entity_Index]
    L[Entity_Index,t] = max(min(Incoming_Order[Entity_Index],OH_Inventory[Entity_Index,t]),Order_flows[Entity_Index,1,t-1])
    L[Entity_Index,t] = Order_flows[Entity_Index,1,t-1]
    
    if (t==1) {
      L_hat[Entity_Index,t] = Order_flows[Entity_Index,1,t]
    } else if (t==2) {
      L_hat[Entity_Index,t-1] = Order_flows[Entity_Index,1,t-1]
      L[Entity_Index,t-1] = L_hat[Entity_Index,t-1]
    } 
    
    L_hat[Entity_Index,t] = theta[Entity_Index]*L[Entity_Index,t] + (1-theta[Entity_Index])*L_hat[Entity_Index,t-1]
    
    S = OH_Inventory[Entity_Index,t] #Stock as of current time
    S = OH_Inventory[Entity_Index,t] - Backorder[Entity_Index,t] #Stock incorporating backorder
    
    
    #For Sterman and and Dogan '15, the L_hat from Sterman '89 is equivalent to the D_p from this paper
    if (StermanDogan15 == TRUE){
      
      #Set up functional equivalence for this single-step subsystem
      D_p = L_hat
      
      if (t==1) {
        D_p[Entity_Index,t] = Order_flows[Entity_Index,1,t]
      } else if (t==2) {
        D_p[Entity_Index,t-1] = Order_flows[Entity_Index,1,t-1]
        L[Entity_Index,t-1] = D_p[Entity_Index,t-1]
      } 
      
      D_p[Entity_Index,t] = theta*L[Entity_Index,t] + (1-theta)*D_p[Entity_Index,t-1]
      
      #Write functional equivalence for this single-step subsystem
      L_hat[Entity_Index,t] = D_p[Entity_Index,t]
      
    }
    
    #For the Oliva '21 model, the outflow is not an exponential smooth
    if (Oliva21 == TRUE){
      L[Entity_Index,t] = Order_flows[Entity_Index,1,t-1]
    }
    
    
    #Add noise to the order if needed
    if (Noisey_Ordering == TRUE) {
      eps = rnorm(1, Noise_Mean, Noise_SD)
    } else {
      eps = 0
    } 
    
    
    
    if ((AI_Entity == TRUE) & (Entity_Index == AI_Entity_Index)) { #AI Decision
      
      ####AI DECISION GOES HERE
      
      if (is.numeric(AI_Order)) {
        
        Order_Placed = AI_Order
        
      } else {
        
        #AI is assumed to use the Sterman '89 model when a parameter dataframe is provided
        Order_Placed = max(0,L_hat[Entity_Index,t]+alpha_s[Entity_Index]*(S_prime[Entity_Index]-S-beta[Entity_Index]*SL))
        
        if (BaseStockAgent == TRUE) {
          
          #Assume supply line is full and calculate the expected inbound orders ahead
          if (Entity_Index != 4) {
            LeadTime = Information_Delay+Shipping_Delay
            OrdersInbound = Order_flows[Entity_Index+1,Information_Delay,max(1,(t-1)):max(1,(t-LeadTime))]
            
            #Populate outbound orders placed for low t assuming we start in steady state
            if (t<=LeadTime) {
              OrdersInbound = c(OrdersInbound,rep(OrdersInbound[1],LeadTime-length(OrdersInbound)))
            }
            
            SupplierBackOrder = Backorder[Entity_Index+1,t]
            
          } else {
            LeadTime = Shipping_Delay #Factory has no information delay
            OrdersInbound = Production_Request[max(1,(t-1)):max(1,(t-LeadTime))]
            
            #Populate outbound orders placed for low t assuming we start in steady state
            if (t<=LeadTime) {
              OrdersInbound = c(OrdersInbound,rep(OrdersInbound[1],LeadTime-length(OrdersInbound)))
            }
            
            SupplierBackOrder = Backorder[Entity_Index,t] #Factory is own supplier
            SupplierBackOrder = 0 #Backorder from factory is double counted otherwise
            
          }
          Total_Orders_Inbound = sum(OrdersInbound,na.rm = TRUE)
          
          ObservedDemand = Order_flows[Entity_Index,1,max(1,(t)):max(1,(t-LeadTime+1))]
          #Populate observed demand estimate for low t assuming we start in steady state
          if (t<=LeadTime) {
            ObservedDemand = c(ObservedDemand,rep(ObservedDemand[1],LeadTime-length(ObservedDemand)))
          }  
          
          AverageDemand = sum(ObservedDemand,na.rm=TRUE)/LeadTime
          SS = Safety_Stock[Entity_Index]
          
          Order_Placed = max(0,SS + LeadTime*AverageDemand + Backorder[Entity_Index,t] - SupplierBackOrder - sum(OrdersInbound[1:(LeadTime-1)]) - OH_Inventory[Entity_Index,t] + eps)
          
        } #End agent base-stock policy
        
        
      }
      
      #########################
      
      
    } else { #'Human Player' decisions or BaseStock decision for AI agent
      
      
      #Default Sterman '89 Ordering
      Order_Placed = max(0,L_hat[Entity_Index,t]+alpha_s[Entity_Index]*(S_prime[Entity_Index]-S-beta[Entity_Index]*SL) + eps)
      
      
      ## Simplified Base Stock Ordering Policy
      
      if (BaseStock == TRUE) {
        #Assume supply line is full and calculate the expected inbound orders ahead
        if (Entity_Index != 4) {
          LeadTime = Information_Delay+Shipping_Delay
          OrdersInbound = Order_flows[Entity_Index+1,Information_Delay,max(1,(t-1)):max(1,(t-LeadTime))]
          
          #Populate outbound orders placed for low t assuming we start in steady state
          if (t<=LeadTime) {
            OrdersInbound = c(OrdersInbound,rep(OrdersInbound[1],LeadTime-length(OrdersInbound)))
          }
          
          SupplierBackOrder = Backorder[Entity_Index+1,t]
          
        } else {
          LeadTime = Shipping_Delay #Factory has no information delay
          OrdersInbound = Production_Request[max(1,(t-1)):max(1,(t-LeadTime))]
          
          #Populate outbound orders placed for low t assuming we start in steady state
          if (t<=LeadTime) {
            OrdersInbound = c(OrdersInbound,rep(OrdersInbound[1],LeadTime-length(OrdersInbound)))
          }
          
          SupplierBackOrder = Backorder[Entity_Index,t] #Factory is own supplier
          SupplierBackOrder = 0 #Backorder from factory is double counted otherwise
          
        }
        Total_Orders_Inbound = sum(OrdersInbound,na.rm = TRUE)
        
        ObservedDemand = Order_flows[Entity_Index,1,max(1,(t)):max(1,(t-LeadTime+1))]
        #Populate observed demand estimate for low t assuming we start in steady state
        if (t<=LeadTime) {
          ObservedDemand = c(ObservedDemand,rep(ObservedDemand[1],LeadTime-length(ObservedDemand)))
        }  
        
        AverageDemand = sum(ObservedDemand,na.rm=TRUE)/LeadTime
        SS = Safety_Stock[Entity_Index]
        
        Order_Placed = max(0,SS + LeadTime*AverageDemand + Backorder[Entity_Index,t] - SupplierBackOrder - sum(OrdersInbound[1:(LeadTime-1)]) - OH_Inventory[Entity_Index,t] + eps)
        
        
      } #End base-stock policy
      
      
      #Oliva '21 Ordering
      
      if (Oliva21 == TRUE){
        
        if (OlivaModel == "Model1" || OlivaModel == "Model2") {
          Order_Placed = max(0,beta_L*L[Entity_Index,t] + gamma_0 + beta_S*S+beta_SL*SL)
        }
        
        
        if (OlivaModel == "Model3" || OlivaModel == "Model4") {
          Order_Placed = max(0,beta_L*L[Entity_Index,t] + gamma_0 + beta_S*S+beta_SL*SL + beta_B*max(0,Backorder[Entity_Index,t]))
        }
        
      }
      
      #Sterman and Dogan '15 Ordering
      
      if (StermanDogan15 == TRUE){
        
        #Original paper assumed particpants had some idea of that the 'true stable demand' was.
        #Not appliable here, so psi may not apply here
        if (Order_Type == "Stationary") {
          D_e = psi*D_c + (1-psi)*D_p[Entity_Index,t]
        } else {
          D_e = D_p[Entity_Index,t]
        }
        
        deliveries = ShipmentsReceived [Entity_Index]
        
        lambda_p = max(Normal_Delay,SL/max(deliveries,1))
        
        lambda_e = min(gamma_max,kappa+omega*lambda_p)
        
        S_star = gamma*D_e
        
        #Model 0
        R_star = D_e
        SL_star = lambda_e*R_star
        
        Order_Placed = max(0,D_e + alpha_s*((S_star - S)+beta*(SL_star-SL)))
        
        
      }
      
    }  
    
    # #TURN ON FOR INTEGER ONLY ORDERING
    if (Integer_Ordering == TRUE) {
      Order_Placed = round(Order_Placed,0)
    }
    
    if (Entity_Index == 4) {
      Production_Request[t] = Order_Placed
    } else {
      Order_flows[Entity_Index+1,Information_Delay,t] = Order_Placed
    }
    
    
  } #next i
  
  # #Debug
  # 
  # Order_flows, Shipping_flows, OH_Inventory, Backorder,
  # L, L_hat, Production_Request
  # 
  # str(Order_flows)
  # str(Shipping_flows)
  # str(OH_Inventory)
  # str(Backorder)
  # str(L)
  # str(L_hat)
  # str(Production_Request)
  
  
  Final_Orders = unname(rbind(Order_flows[2:4,Information_Delay,],Production_Request))
  Amplification = (abs(Final_Orders[,t]-Orders[t]))/Orders[t]
  
  Lagged_Orders = vector()
  for (n in 2:4) {
    frontmatter = rep(NA,((n-1)*Information_Delay))
    Lagged_Orders = rbind(Lagged_Orders,append(frontmatter,Orders[-(length(Orders)-(n-1)*Information_Delay+1):-length(Orders)]))
    
  }
  Lagged_Orders=unname(rbind(Orders,Lagged_Orders))
  
  LaggedAmplifcation = (abs(Final_Orders[,t]-Lagged_Orders[,t]))/Lagged_Orders[,t]
  TotalLaggedAmpCosts = sum(LaggedAmplifcation^2,na.rm=TRUE)
  
  #KEY VARIABLE TO MINIMIZE OVER
  Max_Amp = max(Amplification)
  reward = abs(sum(-25*Amplification^2 + 1))
  #reward = TotalLaggedAmpCosts  
  
  fnt_output = list(Order_flows, Shipping_flows, OH_Inventory, Backorder,
                    L, L_hat, Production_Request, Order_Received, Amplification, reward)
  
  names(fnt_output) = c("Order_flows", "Shipping_flows", "OH_Inventory", "Backorder",
                        "L", "L_hat", "Production_Request","Order_Received",
                        "Amplification", "reward")
  
  return(fnt_output)
  
  
} #END FUNCTION
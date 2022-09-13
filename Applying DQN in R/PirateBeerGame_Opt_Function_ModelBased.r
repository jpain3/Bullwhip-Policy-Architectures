#Function to run the Beer Game for a single time step
#James Paine
#jpaine@mit.edu

PirateBeerGame_Opt = function(AI_Entity = FALSE, AI_Entity_Index=FALSE, AI_parameter_df = FALSE, AI_Order = FALSE, 
                          t, Orders, 
                          Order_flows, Shipping_flows, OH_Inventory, Backorder,
                          L, L_hat, Production_Request,
                          Parameter_df = FALSE, Integer_Ordering = FALSE, Noisey_Ordering = FALSE,
                          Shipping_Delay = 2, Information_Delay = 2
                          ) {

  if (is.numeric(Parameter_df[1,1])) {
    
    #Repair blanks or zero values
    Parameter_df[is.na(Parameter_df)] = 0
    
    
    #Assign parameter values
    theta = unlist(Parameter_df$theta)
    alpha_s = unlist(Parameter_df$alpha_s)
    beta = unlist(Parameter_df$beta)
    S_prime = unlist(Parameter_df$S_prime)
    
    #TeamName = as.character(Parameter_sub$Team_Name[1])
    
  } else {
    
    theta = rep(0.36,4)
    alpha_s = rep(0.26,4)
    beta = rep(0.34,4)
    S_prime = rep(17,4)
    
    TeamName = "Default Average Agents"
    
  }
  
  if (is.numeric(unname(AI_parameter_df[1]))) {

    theta[AI_Entity_Index] = unname(AI_parameter_df["theta"])
    alpha_s[AI_Entity_Index] = unname(AI_parameter_df["alpha_s"])
    beta[AI_Entity_Index] = unname(AI_parameter_df["beta"])
    S_prime[AI_Entity_Index] = unname(AI_parameter_df["S_prime"])

  }
  
  
  #####Receive Inventory and Advance Shipping Delays#####
  
  #Recieve shipments
  
  if (t!= 1) {
    
    OH_Inventory[,t] = OH_Inventory[,t-1] + Shipping_flows[,1,t-1]
    
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
        
        Order_Placed = max(0,L_hat[Entity_Index,t]+alpha_s[Entity_Index]*(S_prime[Entity_Index]-S-beta[Entity_Index]*SL) + eps)
        
      }
      
      #########################
      
      
    } else { #'Human Player' decisions
      
      Order_Placed = max(0,L_hat[Entity_Index,t]+alpha_s[Entity_Index]*(S_prime[Entity_Index]-S-beta[Entity_Index]*SL) + eps)
      
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

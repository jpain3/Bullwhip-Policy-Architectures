#The Pirate's Beer Game
#  Because it's coded in ARRRRR!


#Choose how the teams are selected
#Choose team to simulate. Note Team 0 is the average performance
  Team_Index = 2

# Note, if both set to 'FALSE' then the average fitted values are used
  Random_Team = FALSE
  Random_Entities = FALSE
  #Order_Type = "Rand_Normal"
  Order_Type = "Stationary"
  Order_Type = "Classic"
  
  
####Set ordering controls####
  Integer_Ordering = FALSE
  
  Noisey_Ordering = FALSE
  Noise_Mean = 0 #units of orders/cases
  Noise_SD = 3 #units of orders/cases

  RestrictedPeriods = 0 #Number of rounds in which entities are restricted to ordering specific values
  Backlog_in_Inventory = TRUE #IF set to True then the inventory position of a team is inclusive of backlog    
  
  Stable_Start = TRUE #If set to true, then amount of starting inventory is adjusted to ensure a stable start to a stationary order pattern
  
    
##Define entity order model type
  Oliva21 = FALSE
  OlivaModel = "Model3"

####Define game parameters####
horizon = 52 #How many periods the game will run for
Holding_Cost = 0.5 #dollars for keeping inventory on hand (positive inventory)
Backorder_Cost = 1 #dollars for having outstanding orders (negative inventory)
Shipping_Delay = 2 #weeks
Information_Delay = 2 #weeks
Initial_Inventory = 12 #cases or units


####Import simulation parameters####
#setwd("C:/Users/jpaine\OneDrive/MIT/Beer Game Project/Current Version")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

if (file.exists("JS Parameter Table.csv")) {

  File_Not_Found = 0
  
  Parameter_df = read.csv("JS Parameter Table.csv")
  
  #Repair blanks or zero values
  Parameter_df[is.na(Parameter_df)] = 0
  
  #Put index for a specific team here
  Parameter_subdf = Parameter_df[Parameter_df$Team_Index==Team_Index,]
  TeamName = as.character(Parameter_df[Parameter_df$Team_Index==Team_Index,"Team_Name"][1])
  
  
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


#Other variables and their descriptions:

#O_t = order at time t
#IO_t = Indicated order rate, ie the orders the player needs to place
#L_hat = Expected loss rate of inventory, or the rate at which the player expects inventory to be depleated
  #Modeled here as sinlge backwards looking smoothing function ('adaptive expectations')
  #L_hat_t = theta*L_t-1 + (1-theta)L_hat_t-1
#L = expected loss rate, which is the rate at which the subject received orders
#S_star = #The desired static inventory level
#SL_star = #The desired incomming supply line
  #Note, per page 326, we assume it's constant here but its really a function of the lenght of the delay and the ancipated order rate
#AS_t = Discrepency in the actual stock of material at time t
  #Modeled as = alpha*(S_star - S_t), ie the discrepency between the actual and desired stock
#ASL = Discrepncy in the actual supply line, ie material in transit as of time t
  #Modeled as = alpha_SL*(SL_star - SL_t), ie the discrepency between the actual and desired supply line stock

#For simplification:
#beta = alpha_SL/alpha_s
#S' = S_star + beta*SL_star


#General Ordering Rule:
#O_t = max(0, L_hat_t + alpha_s*(S'-S_t-beta*SL_t)+eps)



###Setting the input order string####



#Customer Order String


if (Order_Type == "Rand_Normal") {

  set.seed(123)
  
  #Normally Distributed demand
  Order_mean = 6
  Order_SD = 2
  Orders = rnorm(horizon+1,Order_mean, Order_SD)
  
  RestrictedOrder = NA

} else if (Order_Type == "Classic"){
  
  #Classic Beer Game
  
  Step_Round = 5
  Orders = append(rep(4,Step_Round),rep(8,((horizon+1)-Step_Round)))
  
  RestrictedOrder = 4
  
  #Second Step
  # Step_Round = 50
  # Orders = append(Orders[1:(Step_Round)],rep(8,(horizon+1) - Step_Round))
  
} else if (Order_Type == "Stationary"){

  #Stationary order signal (mainly used to test and debug)
  
  StationarySignal = 4
  Orders = rep(StationarySignal,horizon+1)
  
  RestrictedOrder = 4
  
  
}

#Enforce stable start if that flag is present
if (Stable_Start == TRUE) {
  
  Initial_Supply_Line = Shipping_Delay*Orders[1]
  
  if (Oliva21 == FALSE){
    Initial_Inventory = S_prime - beta*Initial_Supply_Line
  
    } else {
    
    Initial_Orders = Orders[1]
    Initial_Inventory = ((1-beta_L)*Initial_Orders - beta_SL*Initial_Supply_Line - gamma_0)/(beta_S)
  }
  
}




###Setting up the game####


Order_flows = array(NA,dim = c(4,Information_Delay,(horizon+1)))
#populate initial values of orders
Order_flows[,,1] = Orders[1]

Shipping_flows = array(NA,dim = c(4,Shipping_Delay,(horizon+1)))
#populate initial values of incomming shippments
Shipping_flows[,,1] = Orders[1]

OH_Inventory = array(NA,dim=c(4,(horizon+1)))
OH_Inventory[,1] = Initial_Inventory

Backorder = array(NA,dim=c(4,(horizon+1)))
Backorder[,1] = 0

L_hat = array(NA,dim=c(4,(horizon+1)))
L = array(NA,dim=c(4,(horizon+1)))

Final_Customer_Orders_Filled = rep(NA, (horizon+1))
Production_Request = rep(NA,(horizon+1))
Production_Request[1] = Orders[1]



####Start the game####

# #debug
# t=1
# #t = 11 is interesting

for (t in 2:(horizon+1)) {

  # #debug
  # t = t + 1
  

  #####Recieve Inventory and Advance Shipping Delays#####
  
  #Recieve shipments
  OH_Inventory[,t] = OH_Inventory[,t-1] + Shipping_flows[,1,t-1]
  
  #Advance shipping delays
  Shipping_flows[,1:(Shipping_Delay-1),t] = Shipping_flows[,2:(Shipping_Delay),t-1]
  #Shipping_flows[,1,t] = Shipping_flows[,2,t-1]

  #####Fill Orders######
  
  #View Orders
  Incoming_Order = Order_flows[,1,t-1]
  #Add backlog to order pressure (remove to smooth)
  Incoming_Order = Incoming_Order + Backorder[,t-1]
  
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

  # #debug
  # Entity_Index = 1
  
  for (i in 1:4) {
    
    Entity_Index = i
  
    #Observe supply line
    SL = sum(Shipping_flows[Entity_Index,,t]) #Total supply line inbound
    
    #Observe the current demanded outflow
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
    
    
    #For the Oliva '21 model, the outflow is not an exponential smooth
    if (Oliva21 == TRUE){
      L[Entity_Index,t] = Order_flows[Entity_Index,1,t-1]
    }
    
    S = OH_Inventory[Entity_Index,t] #Stock as of current time
    
    if (Backlog_in_Inventory == TRUE) {
      S = OH_Inventory[Entity_Index,t] - Backorder[Entity_Index,t] #Stock incorporating backlog
    }
    
    #Add noise to the order if needed
    if (Noisey_Ordering == TRUE) {
      eps = rnorm(1, Noise_Mean, Noise_SD)
    } else {
      eps = 0
    } 
    
    #Sterman '89 Ordering
    Order_Placed = max(0,L_hat[Entity_Index,t]+alpha_s[Entity_Index]*(S_prime[Entity_Index]-S-beta[Entity_Index]*SL) + eps)
    
    #Oliva '21 Ordering
    
    if (Oliva21 == TRUE){
      
      if (OlivaModel == "Model1" || OlivaModel == "Model2") {
        Order_Placed = max(0,beta_L*L[Entity_Index,t] + gamma_0 + beta_S*S+beta_SL*SL)
      }
      
 
      if (OlivaModel == "Model3" || OlivaModel == "Model4") {
        Order_Placed = max(0,beta_L*L[Entity_Index,t] + gamma_0 + beta_S*S+beta_SL*SL + beta_B*max(0,Backorder[Entity_Index,t]))
      }

    }

    
    #Restrict orders if required 
    if (t<=RestrictedPeriods  && RestrictedOrder >= 0) {
      Order_Placed = RestrictedOrder
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

  
  # #debug
  # t
  # Order_flows[,,t]
  # Shipping_flows[,,t]
  # OH_Inventory[,t]
  # Production_Request[t]
  # Orders[t]
  # Final_Customer_Orders_Filled[t]
  # OH_Inventory[,t]

  
} #next t




#####Calculate Costs####

Costs_Per_Period = OH_Inventory*Holding_Cost + Backorder*Backorder_Cost
Cummulative_Costs_Per_Period = t(apply( Costs_Per_Period ,1 , cumsum))
Total_Costs_Per_Entity = rowSums(Costs_Per_Period)


Final_Orders = unname(rbind(Order_flows[2:4,Information_Delay,],Production_Request))

Amplification = (abs(Final_Orders-Orders))/Orders

Lagged_Orders = vector()
for (n in 2:4) {
  frontmatter = rep(NA,((n-1)*Information_Delay))
  Lagged_Orders = rbind(Lagged_Orders,append(frontmatter,Orders[-(length(Orders)-(n-1)*Information_Delay+1):-length(Orders)]))
  
}
Lagged_Orders=unname(rbind(Orders,Lagged_Orders))

LaggedAmplifcation = (abs(Final_Orders-Lagged_Orders))/Lagged_Orders
TotalLaggedAmpCosts = sum(LaggedAmplifcation^2,na.rm=TRUE)

#KEY VARIABLE TO MINIMIZE OVER
TotalAmpCosts = abs(sum(-25*Amplification^2 + 1))
Total_Team_Costs = sum(Total_Costs_Per_Entity)
Trimmed_Team_Costs = sum(Costs_Per_Period[,1:36])

#####PLOTS####

legendtext = c("1: Retailer","2: Wholesaler","3: Distributor","4: Factory")

if (Order_Type == "Rand_Normal") {

  savdir = "C://Users//jpaine//OneDrive//MIT//Conferences//2021 - INFORMS//Images//"

} else{
  
  savdir = "C://Users//jpaine//OneDrive//MIT//Conferences//2021 - INFORMS//Images//"
  
}

#AMPLIFICATION FACTOR####
  matplot(t(Amplification), type = 'l', main = paste("Amplifcation -",TeamName),
          ylab = "Amplifcation Ratio Per Round", xlab = "Time"
          )
  legend("topright",legendtext, col=seq_len(4), cex=0.8, fill=seq_len(4))


#Cummulative Costs####
matplot(t(Cummulative_Costs_Per_Period), type = 'l', main = paste("Cummulative Costs -",TeamName),
        ylab = "Cummulative Costs", xlab = "Time", ylim=c(0,round(1.1*max(colSums(Cummulative_Costs_Per_Period)),0))
)
lines(apply(Cummulative_Costs_Per_Period,2,sum),type='l', lty = 4, col = 6, lwd = 3)
legend("topleft",c(legendtext,"TOTAL"), col=c(seq_len(4),6), cex=0.8, fill=c(seq_len(4),6))
  

#BACKLOGGED ORDERS####
  matplot(t(Backorder[,]), type = 'l', main = paste("Backlog -",TeamName),
          ylab = "Backlog Units",xlab="Time"
          )
  legend("topright",legendtext, col=seq_len(4), cex=0.8, fill=seq_len(4))


#ON HAND INVENTORY####
  matplot(t(OH_Inventory[,]), type = 'l', main = paste("On Hand Inventory -",TeamName), lwd = 3, lty = 1,
          ylab = "On Hand Inventory", xlab = "Time"
  )
  legend("topright",legendtext, col=seq_len(4), cex=0.8, fill=seq_len(4))

  
#fullsavpath = paste(savdir,"NET INVENTORY ",TeamName,".png",sep="")  
#png(file=fullsavpath,height=460,width=586)
#NET INVENTORY####
  matplot(t((OH_Inventory[,]-Backorder[,])), type = 'l', main = paste("Net Inventory -",TeamName), lwd = 3, lty = 1,
          ylab = "Net Inventory", xlab = "Time",
          ylim = c(-40,70)
          )
  legend("topright",legendtext, col=seq_len(4), cex=0.8, fill=seq_len(4))
#dev.off()


fullsavpath = paste(savdir,"ORDERS ",TeamName,".png",sep="")  
png(file=fullsavpath,height=690,width=870)
#ORDERS PLACED PER ROUND####
ymax = round(max(Final_Orders)*1.1,0)
matplot(t(Final_Orders[,]), type = 'l', main = paste("Orders -",TeamName), lwd = 3, lty = 1,
        ylim = c(0,ymax),
        ylab = "Units Ordered Each Round", xlab = "Time"
)
lines(Orders, type='l', lty = 4, col = 8, lwd = 3)
legend("topright",c("Customer",legendtext), col=c(8,seq_len(4)), cex=0.8, fill=c(8,seq_len(4)))
dev.off()

#Sequential Orders Cust only
ymax = round(max(Final_Orders)*1.1,0)
plot(Orders, type='l', lty = 4, col = 8, lwd = 3,
     ylim = c(0,ymax),
     ylab = "Units Ordered Each Round", xlab = "Time"
)
legend("topright",c("Customer",legendtext), col=c(8,seq_len(4)), cex=0.8, fill=c(8,seq_len(4)))


#Sequential Orders Cust+1
ymax = round(max(Final_Orders)*1.1,0)
plot(Final_Orders[1,], type = 'l', lwd = 3, lty = 1,
        ylim = c(0,ymax),
        ylab = "Units Ordered Each Round", xlab = "Time"
)
lines(Orders, type='l', lty = 4, col = 8, lwd = 3)
legend("topright",c("Customer",legendtext), col=c(8,seq_len(4)), cex=0.8, fill=c(8,seq_len(4)))

#Sequential Orders 2-4
ymax = round(max(Final_Orders)*1.1,0)
matplot(t(Final_Orders[1:2,]), type = 'l', lwd = 3, lty = 1,
        ylim = c(0,ymax),
        ylab = "Units Ordered Each Round", xlab = "Time"
)
lines(Orders, type='l', lty = 4, col = 8, lwd = 3)
legend("topright",c("Customer",legendtext), col=c(8,seq_len(4)), cex=0.8, fill=c(8,seq_len(4)))


#dev.off()

####Write OUTPUTS####
TeamName
TotalAmpCosts
TotalLaggedAmpCosts
Total_Team_Costs
Trimmed_Team_Costs

# n=1
# report_entity = as.integer(substr(TeamName, nchar(TeamName)-n+1, nchar(TeamName)))

# write.table(t(c(TeamName, as.vector(Parameter_subdf[report_entity,5:8]),horizon,Total_Team_Costs,TotalAmpCosts)),"clipboard",sep="\t",row.names=FALSE,col.names=FALSE)
# write.table(Final_Orders,"clipboard",sep="\t",row.names=FALSE,col.names=FALSE)
# write.table(OH_Inventory,"clipboard",sep="\t",row.names=FALSE,col.names=FALSE)
# write.table(Backorder,"clipboard",sep="\t",row.names=FALSE,col.names=FALSE)
# Function to run the Beer Game for a single time step

# It's the Pirate Beer Game because it was originally coded in ARRRR!

import numpy as np


def PirateBeerGame_funct(AI_Entity_Index, AI_Order, Orders, Order_flows, Shipping_flows, OH_Inventory,
                         Backorder, L_hat, Production_Request, AI_Entity=False, AI_parameter_df=False,
                         Integer_Ordering=False, Noisey_Ordering=False, Noise_Mean=0, Noise_SD=1,
                         Parameter_df=False, Shipping_Delay=2, Information_Delay=2):
    ##DEBUG ONLY
    # Shipping_Delay = 2
    # Information_Delay = 2
    # AI_Entity = False
    # AI_Entity_Index = 3
    # t = 0
    # OH_Inventory = [12]*4
    # Shipping_flows = [[4]*2]*4
    # Order_flows = [[4]*2]*4
    # Backorder = [0]*4
    # Orders = 4
    # Production_Request = 4
    # L_hat = [4.00]*4

    Final_Orders = np.empty(4, dtype=float)
    OH_Inventory = np.array(OH_Inventory)
    Shipping_flows = np.array(Shipping_flows)
    Order_flows = np.array(Order_flows)

    # Ensure that the order flow facing the retailer is the actual customer order
    Order_flows[0, 0] = Orders

    # Read in the ordering paramters
    if Parameter_df != False:
        theta = Parameter_df['theta']
        alpha_s = Parameter_df['alpha_s']
        beta = Parameter_df['beta']
        S_prime = Parameter_df['S_prime']
    else:
        theta = [0.36] * 4
        alpha_s = [0.26] * 4
        beta = [0.34] * 4
        S_prime = [17] * 4

        TeamName = "Default Average Agents"

    # Read in AI Ordering Parameters if present
    if AI_parameter_df != False:
        theta[AI_Entity_Index] = AI_parameter_df['theta']
        alpha_s[AI_Entity_Index] = AI_parameter_df['alpha_s']
        beta[AI_Entity_Index] = AI_parameter_df['beta']
        S_prime[AI_Entity_Index] = AI_parameter_df['S_prime']

    #####Recieve Inventory and Advance Shipping Delays#####

    # Recieve shipments
    New_OH_Inventory = OH_Inventory + Shipping_flows[:, 0]

    # Advance shippping delays
    Shipping_flows[:, 0] = Shipping_flows[:, (Shipping_Delay - 1)]
    #Shipping_flows[:, (Shipping_Delay - 1)] = np.nan

    #####Fill Orders######

    # View Orders
    Order_Received = Order_flows[:, 0]
    # Calculate net order that needs to be fullfilled
    Incoming_Order = Order_flows[:, 0] + Backorder
    # Ship what you can
    Outbound_shipments = np.maximum(0, np.minimum(New_OH_Inventory, Incoming_Order))

    # Put shipments into lefthand shipping slot
    Shipping_flows[0:3, 1] = Outbound_shipments[1:]

    # Send shipments from retailer to the final customer
    Final_Customer_Orders_Filled = Outbound_shipments[0]

    # Update the On-Hand Inventory to account for outflows
    OH_Inventory = New_OH_Inventory - Outbound_shipments

    # Determine Backlog, if any
    Inventory_Shortage = Order_flows[:, 0] - New_OH_Inventory
    New_Backorder = np.maximum(0, Backorder + Inventory_Shortage)
    Backorder = np.copy(New_Backorder)

    # Remember observed order but then Overwrite processed order flow to NaN for debuging if needed
    if Orders == 8:
        Orders = Orders

    Observed_Order = np.copy(Order_flows[:, 0])
    #Order_flows[:, 0] = np.nan

    #####Advance Order Slips and Brewers Brew######

    # Advance order slips
    Order_flows[:, 0] = Order_flows[:, (Information_Delay - 1)]
    #Order_flows[:, (Information_Delay - 1)] = np.nan

    # Brewers Brew
    Shipping_flows[3, (Shipping_Delay - 1)] = Production_Request

    #####PLACE ORDERS######

    for i in range(0, 4):

        Entity_Index = i

        # Obsrve the total supply line and the previous demand
        SL = sum(Shipping_flows[Entity_Index, :])
        L = Observed_Order[Entity_Index]

        # L hat is smoothing of observed demand from previous 2 periods
        #if t == 0:
        #    L_hat[Entity_Index] = np.copy(Observed_Order[Entity_Index])

        # Update L_hat (expected future orders) based on observed order
        L_hat_new = theta[Entity_Index] * L + (1 - theta[Entity_Index]) * L_hat[Entity_Index]
        L_hat[Entity_Index] = L_hat_new

        # Note stock of current inventory
        S = OH_Inventory[Entity_Index]

        #Note stock of current inventory inclusive of backorder position
        S = OH_Inventory[Entity_Index] - Backorder[Entity_Index]

        # Add noise to the order if needed
        if (Noisey_Ordering == True):
            eps = np.random.normal(Noise_Mean, Noise_SD)
        else:
            eps = 0

        # AI Decision
        if (AI_Entity == True) and (Entity_Index == AI_Entity_Index):
            if (AI_Order != False):
                Order_Placed = AI_Order
            else:
                Order_Placed = max(0, L_hat[Entity_Index] + alpha_s[Entity_Index] * (
                            S_prime[Entity_Index] - S - beta[Entity_Index] * SL) + eps)


        else:
            Order_Placed = max(0, L_hat[Entity_Index] + alpha_s[Entity_Index] * (
                        S_prime[Entity_Index] - S - beta[Entity_Index] * SL) + eps)

        ##TURN ON FOR INTEGER ONLY ORDERING
        if Integer_Ordering == True:
            Order_Placed = round(Order_Placed, 0)

        if Entity_Index == 3:
            Production_Request = Order_Placed
        else:
            Order_flows[Entity_Index + 1, (Information_Delay - 1)] = Order_Placed

    # End of loop

    # Make orders placed by each entity explict
    Final_Orders[0:3] = Order_flows[1:, (Information_Delay - 1)]
    Final_Orders[3] = Production_Request

    Amplification = (Final_Orders - Orders) / Orders

    # Key variable ot minimize over
    Max_Amp = max(Amplification)
    reward = sum(-25 * np.power(Amplification, 2))

    # fnt_output = list(Order_flows, Shipping_flows, OH_Inventory, Backorder, L_hat, Production_Request, Amplification, reward)
    fnt_output = {"Order_flows": Order_flows, "Shipping_flows": Shipping_flows, "OH_Inventory": OH_Inventory,
                  "Backorder": Backorder, "L_hat": L_hat, "Production_Request": Production_Request,
                  "Entity_Orders": Final_Orders, "Final_Customer_Orders_Filled": Final_Customer_Orders_Filled,
                  "Amplification": Amplification, "reward": reward, "Order_Received": Order_Received}

    return fnt_output


# Function to reset the game to default parameters
def reset_game(horizon=36, Initial_Inventory=12, Information_Delay=2, Shipping_Delay=2):
    # Customer Order String
    # Classic Beer Game
    Step_Round = 4
    Orders = ([4] * Step_Round) + ([9] * (horizon - Step_Round))

    Second_Step = 150
    Orders = Orders[0:Second_Step] + ([9] * (horizon - Second_Step))

    ##################
    # Setting up the game
    ##################

    Order_flows = np.full([4, 2], Orders[0], dtype=float)
    Shipping_flows = np.full([4, 2], Orders[0], dtype=float)
    OH_Inventory = [Initial_Inventory] * 4
    Backorder = [0] * 4
    L_hat = [Orders[0]] * 4
    Order_History = np.full([4, horizon], 0, dtype=float)
    Service_rate = [0] * horizon
    OH_Inventory_History = np.full([4, horizon], 0, dtype=float)
    Backlog_History = np.full([4, horizon], 0, dtype=float)
    Production_Request = Orders[0]

    Amp_Vector = [0] * horizon
    Reward_Vector = [0] * horizon

    Output = {"Orders": Orders, "Order_flows": Order_flows, "Shipping_flows": Shipping_flows,
              "OH_Inventory": OH_Inventory, "Backorder": Backorder, "L_hat": L_hat,
              "Order_History": Order_History, "Service_rate": Service_rate,
              "OH_Inventory_History": OH_Inventory_History, "Backlog_History": Backlog_History,
              "Production_Request": Production_Request, "Amp_Vector": Amp_Vector, "Reward_Vector": Reward_Vector}

    return (Output)


if __name__ == "__main__":
    horizon = 104

    reset_list = reset_game(horizon=horizon)
    locals().update(reset_list)

    Parameter_df = {"theta": [0.36] * 4,
                    "alpha_s": [0.26] * 4,
                    "beta": [0.34] * 4,
                    "S_prime": [17] * 4}

    Holding_Cost = 0.50
    Backorder_Cost = 1.00

    AI_Entity = False
    AI_Entity_Index = False
    AI_Order = False
    Integer_Ordering = True
    Noisey_Ordering = False

    for t in range(0, (horizon)):

        if t >= 20:
            t = t

        # NESTED FUNCTION TO RUN THE GAME FOR ONE TIME STEP AND RETURN THE NEW STATE
        BeerGame_output = PirateBeerGame_funct(AI_Entity_Index=AI_Entity_Index, AI_Order=AI_Order, Orders=Orders[t],
                                               Order_flows=Order_flows,
                                               Shipping_flows=Shipping_flows, OH_Inventory=OH_Inventory,
                                               Backorder=Backorder, L_hat=L_hat,
                                               Production_Request=Production_Request, AI_Entity=AI_Entity,
                                               Noisey_Ordering=Noisey_Ordering, Integer_Ordering=Integer_Ordering,
                                               Parameter_df=Parameter_df)

        locals().update(BeerGame_output)

        # Write values for analysis/plotting later
        Order_History[:, t] = Entity_Orders
        OH_Inventory_History[:, t] = OH_Inventory
        Backlog_History[:, t] = Backorder
        Service_rate[t] = Final_Customer_Orders_Filled / Orders[t]

    # Calculate costs
    Net_Inventory = OH_Inventory_History - Backlog_History
    Costs_Per_Period = OH_Inventory_History * Holding_Cost + Backlog_History * Backorder_Cost
    Total_Costs_Per_Entity = np.sum(Costs_Per_Period, 1)
    Total_Team_Costs = sum(Total_Costs_Per_Entity)

    print(Order_History)
    print(Total_Team_Costs)

    ###GRAPHS###
    import matplotlib.pyplot as plt

    x = range(0, horizon)

    plt.figure(1)
    PlotObj = plt.plot(Order_History.T)
    plt.title('Orders per Period')
    plt.xlabel('Time')
    plt.ylabel('Orders')
    # showing legend
    plt.legend(iter(PlotObj), ('0: Retailer', '1: Wholesaler', '2: Distributor', '3: Factory'))

    plt.figure(2)
    PlotObj = plt.plot(Net_Inventory.T)
    plt.title('Net Inventory per Period')
    plt.xlabel('Time')
    plt.ylabel('Net Inventory (On-Hand less Backlog)')
    # showing legend
    plt.legend(iter(PlotObj), ('0: Retailer', '1: Wholesaler', '2: Distributor', '3: Factory'))

    plt.show()